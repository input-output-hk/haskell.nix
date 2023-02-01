let
  # Here we try to figure out which qemu to use based on the host platform.
  # This guess can be overridden by passing qemuSuffix
  qemuByHostPlatform = hostPlatform:
    # I'd prefer this was a dictionary lookup, with a fall through into abort,
    # that would make this more readable I guess.  I think there is some similar
    # mapping somewhere in haskell.nix
    if hostPlatform.isAarch32
    then "arm"
    else if hostPlatform.isAarch64
    then "aarch64"
    else abort "Don't know which QEMU to use for hostPlatform ${hostPlatform.config}. Please provide qemuSuffix";
in
{ stdenv
, lib
, writeScriptBin
, qemu
, qemuSuffix ? (qemuByHostPlatform hostPlatform)
, iserv-proxy
, iserv-proxy-interpreter
, gmp
, extra-test-libs ? []
, buildPlatform
, hostPlatform
, ...
}:
let

  # we want this to hold only for arm (32 and 64bit) for now.
  # For 32bit android, we need to pass -no-pie, as we otherwise
  # get -pie injected into the linker flags. We don't want that.
  # If we target 32bit android, we need remote-iserv to be runnable
  # in a 32bit linux (via qemu-arm user mode emulation).  If we have
  # -pie enabled, it will produce a static-pie executable, which
  # seems a lot like what we want but will crash on launch.  It appears
  # the the __stack_chk_guard lookups go through some lookup table, and
  # while the relocations for the lookup table are correct, the __stack_chk_guard
  # address isn't properly relocated.  This could also be because libc isn't
  # supposed to be staticlly linked really.  However because we are lacking
  # the loader for arm on linux, we can't used dynamically linked executables
  # until one in /system/bin/linker is provided.
  #
  # We also need to run armv7a-android in unshare --user --pid --fork, to
  # ensure that we get a low pid < 65535 for android (If we run outside)
  # of nix build envs.
  isLinuxCross = buildPlatform != hostPlatform && hostPlatform.isLinux && (hostPlatform.isAarch32 || hostPlatform.isAarch64);
  qemuIservWrapper = writeScriptBin "iserv-wrapper" ''
    #!${stdenv.shell}
    set -euo pipefail
    # Unset configure flags as configure should have run already
    unset configureFlags
    PORT=$((5000 + $RANDOM % 5000))
    (>&2 echo "---> Starting ${iserv-proxy-interpreter.exeName} on port $PORT")
    ${qemu}/bin/qemu-${qemuSuffix} ${iserv-proxy-interpreter.override (lib.optionalAttrs hostPlatform.isAndroid { setupBuildFlags = ["--ghc-option=-optl-static" ] ++ lib.optional hostPlatform.isAarch32 "--ghc-option=-optl-no-pie";})}/bin/remote-iserv tmp $PORT &
    (>&2 echo "---| ${iserv-proxy-interpreter.exeName} should have started on $PORT")
    RISERV_PID="$!"
    ${iserv-proxy}/bin/iserv-proxy $@ 127.0.0.1 "$PORT"
    (>&2 echo "---> killing ${iserv-proxy-interpreter.exeName}...")
    kill $RISERV_PID
    '';
  configureFlags = lib.optional hostPlatform.isAarch32 "--disable-split-sections";
  setupBuildFlags = map (opt: "--ghc-option=" + opt) ((lib.optionals isLinuxCross
    [ "-fexternal-interpreter"
      "-pgmi" "${qemuIservWrapper}/bin/iserv-wrapper"
      "-L${gmp}/lib"
         # Required to work-around https://gitlab.haskell.org/ghc/ghc/issues/15275
    ] ++ lib.optionals hostPlatform.isAarch64 ["-fPIC"]))
    ++ lib.optionals hostPlatform.isAarch32 (map (opt: "--gcc-option=" + opt) [ "-fno-pic" "-fno-plt" ])
       # Also for GHC #15275
    ++ lib.optionals hostPlatform.isAarch64 ["--gcc-option=-fPIC"];
  qemuTestWrapper = writeScriptBin "test-wrapper" ''
    #!${stdenv.shell}
    set -euo pipefail
    ${qemu}/bin/qemu-${qemuSuffix} $@*
    '';
  testWrapper = lib.optional isLinuxCross "${qemuTestWrapper}/bin/test-wrapper";

  preCheck = lib.optionalString isLinuxCross ''
    echo "================================================================="
    echo "RUNNING TESTS for $name via qemu-${qemuSuffix}"
    echo "================================================================="
    echo "Copying extra test libraries"
    for p in ${lib.concatStringsSep " " extra-test-libs}; do
      find "$p" -iname '*.so*' -exec cp {} . \;
    done
  '';
  postCheck = lib.optionalString isLinuxCross ''
    echo "================================================================="
    echo "END RUNNING TESTS"
    echo "================================================================="
  '';

  enableShared = lib.mkDefault (!isLinuxCross);

in { inherit preCheck postCheck configureFlags setupBuildFlags testWrapper enableShared; }
