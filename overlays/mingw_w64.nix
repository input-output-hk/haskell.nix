# Cross compilation logic.
# Returns override fields for use with nix-tools.
{ lib
, writeShellScriptBin
, wine
, mingw_w64_pthreads
, iserv-proxy
, iserv-proxy-interpreter
, iserv-proxy-interpreter-prof
, gmp
, hostPlatform
, symlinkJoin
}:
let

  configureFlags = ["--disable-split-sections"];

  wineIservWrapperScript = enableProfiling:
    let
      interpreter =
        if enableProfiling
          then iserv-proxy-interpreter-prof
          else iserv-proxy-interpreter;
      no-load-call = lib.optionalString (interpreter.exeName != "remote-iserv.exe") "--no-load-call";
    in
      writeShellScriptBin ("iserv-wrapper" + lib.optionalString enableProfiling "-prof") ''
        set -euo pipefail
        ISERV_ARGS=''${ISERV_ARGS:-}
        PROXY_ARGS=''${PROXY_ARGS:-}
        # unset the configureFlags.
        # configure should have run already
        # without restting it, wine might fail
        # due to a too large environment.
        unset configureFlags
        unset configurePhase
        WINEPREFIX=''${WINEPREFIX:-$(mktemp -d)}
        REMOTE_ISERV=''${REMOTE_ISERV:-$(mktemp -d)}
        PORT=$((5000 + $RANDOM % 5000))
        (>&2 echo "---> Starting ${interpreter.exeName} on port $PORT")
        REMOTE_ISERV=$(mktemp -d)
        ln -s ${interpreter}/bin/* $REMOTE_ISERV
        # See coment in comp-builder.nix for where this comes from and why it's here
        # TODO use `LINK_DLL_FOLDERS` here once it is in all the nixpkgs we want to support.
        for p in $pkgsHostTargetAsString; do
          find "$p" -iname '*.dll' -exec ln -sf {} $REMOTE_ISERV \;
          find "$p" -iname '*.dll.a' -exec ln -sf {} $REMOTE_ISERV \;
        done
        # Some DLLs have a `lib` prefix but we attempt to load them without the prefix.
        # This was a problem for `double-conversion` package when used in TH code.
        # Creating links from the `X.dll` to `libX.dll` works around this issue.
        (
        cd $REMOTE_ISERV
        for l in lib*.dll; do
          ln -s "$l" "''${l#lib}"
        done
        )
        echo "To re-use the same wine-prefix and remote-iserv, set the following environment variables:"
        echo "export WINEPREFIX=$WINEPREFIX"
        echo "export REMOTE_ISERV=$REMOTE_ISERV"
        # Not sure why this `unset` helps.  It might avoids some kind of overflow issue.  We see `wine` fail to start when building `cardano-wallet-cli` test `unit`.
        unset pkgsHostTargetAsString
        unset LINK_DLL_FOLDERS
        WINEDLLOVERRIDES="winemac.drv=d" WINEDEBUG=warn-all,fixme-all,-menubuilder,-mscoree,-ole,-secur32,-winediag WINEPREFIX=$TMP ${wine}/bin/wine64 $REMOTE_ISERV/${interpreter.exeName} tmp $PORT ${no-load-call} $ISERV_ARGS &
        (>&2 echo "---| ${interpreter.exeName} should have started on $PORT")
        RISERV_PID="$!"
        ISERV_TARGET=WINE ${iserv-proxy}/bin/iserv-proxy $@ 127.0.0.1 "$PORT" ${no-load-call} $PROXY_ARGS
        (>&2 echo "---> killing ${interpreter.exeName}...")
        kill $RISERV_PID
      '';

  wineIservWrapper = symlinkJoin { name = "iserv-wrapper"; paths = [ (wineIservWrapperScript false) (wineIservWrapperScript true) ]; };

  ################################################################################
  # Build logic (TH support via remote iserv via wine)
  #
  ghcOptions = [
    "-fexternal-interpreter"
    "-pgmi" "${wineIservWrapper}/bin/iserv-wrapper"
    # TODO: this should be automatically injected based on the extraLibrary.
    "-L${mingw_w64_pthreads}/lib"
    "-L${mingw_w64_pthreads}/bin"
    "-L${gmp}/lib"
    ];

  ################################################################################
  # Test logic via wine
  #
  wineTestWrapper = writeShellScriptBin "test-wrapper" ''
    set -euo pipefail
    export WINEDLLOVERRIDES="winemac.drv=d"
    export WINEDEBUG=warn-all,fixme-all,-menubuilder,-mscoree,-ole,-secur32,-winediag
    export LC_ALL=en_US.UTF-8
    export WINEPREFIX=$TMP
    Path="''${Path:-}"
    unset configureFlags
    unset configurePhase
    unset LINK_DLL_FOLDERS
    for path in ''${nativeBuildInputs:-}; do
      if [ -d "$path/bin" ]; then
        Path="$Path;$(${wine}/bin/winepath -w $path/bin)";
      fi
    done
    export Path
    ${wine}/bin/wine64 $@
  '';
  testWrapper = ["${wineTestWrapper}/bin/test-wrapper"];

in { inherit testWrapper ghcOptions configureFlags; }
