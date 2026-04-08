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
    (>&2 echo "---> Starting iserv-proxy with piped ${interpreter.exeName} (qemu: ${qemu}/bin/qemu-${qemuSuffix})")
    ${iserv-proxy}/bin/iserv-proxy $@ --pipe ${qemu}/bin/qemu-${qemuSuffix} ${interpreter}/bin/${interpreter.exeName} tmp --stdio $ISERV_ARGS
    '';
  qemuIservWrapper = symlinkJoin { name = "iserv-wrapper"; paths = [ (qemuIservWrapperScript false) (qemuIservWrapperScript true) ]; };
  configureFlags = lib.optional (hostPlatform.isAarch32 || hostPlatform.isAndroid) "--disable-split-sections";
  ghcOptions =
    [ "-fexternal-interpreter"
      "-pgmi" "${qemuIservWrapper}/bin/iserv-wrapper"
      "-L${gmp}/lib"
         # Required to work-around https://gitlab.haskell.org/ghc/ghc/issues/15275
    ] ++ lib.optionals hostPlatform.isAarch64 ["-fPIC"]
      # The GHC RTS references dlopen/dlclose/dlsym/dlerror even with
      # -dynamic-system-linker disabled. Link -ldl for android.
      ++ lib.optionals hostPlatform.isAndroid ["-optl-ldl"];

  # Wrapper for qemu testing
  qemuTestWrapper = writeShellScriptBin "test-wrapper" ''
    set -euo pipefail
    ${qemu}/bin/qemu-${qemuSuffix} $@*
  '';

  # Choose the appropriate test wrapper
  testWrapper = [ "${qemuTestWrapper}/bin/test-wrapper" ];

in { inherit configureFlags ghcOptions testWrapper; }
