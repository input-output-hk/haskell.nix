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

  # we want this to hold only for arm (32 and 64bit) for now.
  isLinuxCross = haskellLib.isCrossHost && hostPlatform.isLinux && (hostPlatform.isAarch32 || hostPlatform.isAarch64);
  qemuIservWrapperScript = enableProfiling:
    let
      interpreter =
        if enableProfiling
          then iserv-proxy-interpreter.override { inherit enableProfiling; }
          else iserv-proxy-interpreter;
    in
      writeShellScriptBin ("iserv-wrapper" + lib.optionalString enableProfiling "-prof") ''
    set -euo pipefail
    ISERV_ARGS=''${ISERV_ARGS:-}
    PROXY_ARGS=''${PROXY_ARGS:-}
    # Unset configure flags as configure should have run already
    unset configureFlags
    PORT=$((5000 + $RANDOM % 5000))
    (>&2 echo "---> Starting ${interpreter.exeName} on port $PORT")
    ${qemu}/bin/qemu-${qemuSuffix} ${interpreter.override
      (lib.optionalAttrs hostPlatform.isAndroid {
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
  qemuTestWrapper = writeShellScriptBin "test-wrapper" ''
    set -euo pipefail
    ${qemu}/bin/qemu-${qemuSuffix} $@*
    '';
  testWrapper = lib.optional isLinuxCross "${qemuTestWrapper}/bin/test-wrapper";

  enableShared = lib.mkDefault (!isLinuxCross);

in { inherit configureFlags setupBuildFlags testWrapper enableShared; }
