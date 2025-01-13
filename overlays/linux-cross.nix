{ stdenv
, lib
, haskellLib
, writeShellScriptBin
, qemu
, qemuSuffix ? (haskellLib.qemuByHostPlatform hostPlatform)
, iserv-proxy
, iserv-proxy-interpreter
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

  # we want this to hold only for arm (32 and 64bit) for now.
  isLinuxCross = haskellLib.isCrossHost && hostPlatform.isLinux && (hostPlatform.isAarch32 || hostPlatform.isAarch64 || hostPlatform.isi686);
  qemuIservWrapperScript = enableProfiling:
    let
      interpreter =
        if enableProfiling
          then iserv-proxy-interpreter.override { inherit enableProfiling; }
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
    ${qemu}/bin/qemu-${qemuSuffix} ${interpreter.override
      ({
        patches =    lib.optional (builtins.compareVersions interpreter.version "9.0" > 0 && hostPlatform.isAndroid && hostPlatform.isAarch32) ./patches/iserv-proxy-interpreter-9.3-android32.patch
                  ++ lib.optional (builtins.compareVersions interpreter.version "9.0" > 0 && hostPlatform.isAndroid && hostPlatform.isAarch64) ./patches/iserv-proxy-interpreter-9.3-android.patch
                  ;
       } // lib.optionalAttrs hostPlatform.isAndroid {
        setupBuildFlags = ["--ghc-option=-optl-static" ] ++ lib.optional hostPlatform.isAarch32 "--ghc-option=-optl-no-pie";
        enableDebugRTS = true;
       })}/bin/${interpreter.exeName} tmp $PORT $ISERV_ARGS &
    (>&2 echo "---| ${interpreter.exeName} should have started on $PORT")
    RISERV_PID="$!"
    ${iserv-proxy}/bin/iserv-proxy $@ 127.0.0.1 "$PORT" $PROXY_ARGS
    (>&2 echo "---> killing ${interpreter.exeName}...")
    kill $RISERV_PID
    '';
  qemuIservWrapper = symlinkJoin { name = "iserv-wrapper"; paths = [ (qemuIservWrapperScript false) (qemuIservWrapperScript true) ]; };
  configureFlags = lib.optional (hostPlatform.isAarch32 || hostPlatform.isAndroid) "--disable-split-sections";
  setupBuildFlags = map (opt: "--ghc-option=" + opt) ((lib.optionals isLinuxCross
    [ "-fexternal-interpreter"
      "-pgmi" "${qemuIservWrapper}/bin/iserv-wrapper"
      "-L${gmp}/lib"
         # Required to work-around https://gitlab.haskell.org/ghc/ghc/issues/15275
    ] ++ lib.optionals hostPlatform.isAarch64 ["-fPIC"]))
    ++ lib.optionals hostPlatform.isAarch32 (map (opt: "--gcc-option=" + opt) [ "-fno-pic" "-fno-plt" ])
       # Also for GHC #15275
    ++ lib.optionals hostPlatform.isAarch64 ["--gcc-option=-fPIC"];

  # Wrapper to output a warning for Android
  qemuNotSupportedWarning = writeShellScriptBin "warning-wrapper" ''
    echo "Warning: Running Android apps on Linux using qemu is not supported." >&2
  '';

  # Wrapper for qemu testing
  qemuTestWrapper = writeShellScriptBin "test-wrapper" ''
    set -euo pipefail
    ${qemu}/bin/qemu-${qemuSuffix} $@*
  '';

  # Choose the appropriate test wrapper
  testWrapper = lib.optional isLinuxCross
    (if hostPlatform.isAndroid
      then "${qemuNotSupportedWarning}/bin/warning-wrapper"
      else "${qemuTestWrapper}/bin/test-wrapper");

  enableShared = lib.mkDefault (!isLinuxCross);

in { inherit configureFlags setupBuildFlags testWrapper enableShared; }
