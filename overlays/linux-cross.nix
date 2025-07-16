{ stdenv
, lib
, haskellLib
, writeShellScriptBin
, qemu
, qemuSuffix ? (haskellLib.qemuByHostPlatform hostPlatform)
, iserv-proxy
, iserv-proxy-interpreter
, iserv-proxy-interpreter-prof
, gmp
, buildPlatform
, hostPlatform
, symlinkJoin
, ...
}:
let
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

  qemuIservWrapperScript = enableProfiling:
    let
      interpreter =
        if enableProfiling
          then iserv-proxy-interpreter-prof
          else iserv-proxy-interpreter;
    in
      writeShellScriptBin ("iserv-wrapper" + lib.optionalString enableProfiling "-prof") ''
    #!${stdenv.shell}
    set -euo pipefail
    ISERV_ARGS=''${ISERV_ARGS:-}
    PROXY_ARGS=''${PROXY_ARGS:-}
    # Unset configure flags as configure should have run already
    unset configureFlags
    PORT=$((5000 + $RANDOM % 5000))
    (>&2 echo "---> Starting ${interpreter.exeName} on port $PORT")
    ${qemu}/bin/qemu-${qemuSuffix} ${interpreter}/bin/${interpreter.exeName} tmp $PORT $ISERV_ARGS &
    (>&2 echo "---| ${interpreter.exeName} should have started on $PORT")
    RISERV_PID="$!"
    ${iserv-proxy}/bin/iserv-proxy $@ 127.0.0.1 "$PORT" $PROXY_ARGS
    (>&2 echo "---> killing ${interpreter.exeName}...")
    kill $RISERV_PID
    '';
  qemuIservWrapper = symlinkJoin { name = "iserv-wrapper"; paths = [ (qemuIservWrapperScript false) (qemuIservWrapperScript true) ]; };
  configureFlags = lib.optional (hostPlatform.isAarch32 || hostPlatform.isAndroid) "--disable-split-sections";
  ghcOptions =
    [ "-fexternal-interpreter"
      "-pgmi" "${qemuIservWrapper}/bin/iserv-wrapper"
      "-L${gmp}/lib"
         # Required to work-around https://gitlab.haskell.org/ghc/ghc/issues/15275
    ] ++ lib.optionals hostPlatform.isAarch64 ["-fPIC"];

  # Wrapper for qemu testing
  qemuTestWrapper = writeShellScriptBin "test-wrapper" ''
    set -euo pipefail
    ${qemu}/bin/qemu-${qemuSuffix} $@*
  '';

  # Choose the appropriate test wrapper
  testWrapper = "${qemuTestWrapper}/bin/test-wrapper";

in { inherit configureFlags ghcOptions testWrapper; }
