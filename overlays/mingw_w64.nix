# Cross compilation logic.
# Returns override fields for use with nix-tools.
{ stdenv
, lib
, writeScriptBin
, wine
, mingw_w64_pthreads
, iserv-proxy
, remote-iserv
, gmp
# extra libraries. Their dlls are copied
# when tests are run.
, extra-test-libs ? []
, hostPlatform
, symlinkJoin
}:
let

  configureFlags = lib.optional hostPlatform.isWindows "--disable-split-sections";

  wineIservWrapperScript = enableProfiling: writeScriptBin ("iserv-wrapper" + lib.optionalString enableProfiling "-prof") ''
    #!${stdenv.shell}
    set -euo pipefail
    # unset the configureFlags.
    # configure should have run already
    # without restting it, wine might fail
    # due to a too large environment.
    unset configureFlags
    PORT=$((5000 + $RANDOM % 5000))
    (>&2 echo "---> Starting remote-iserv on port $PORT")
    REMOTE_ISERV=$(mktemp -d)
    ln -s ${if enableProfiling then remote-iserv.override { inherit enableProfiling; } else remote-iserv}/bin/* $REMOTE_ISERV
    # See coment in comp-builder.nix for where this comes from and why it's here
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
    WINEDLLOVERRIDES="winemac.drv=d" WINEDEBUG=warn-all,fixme-all,-menubuilder,-mscoree,-ole,-secur32,-winediag WINEPREFIX=$TMP ${wine}/bin/wine64 $REMOTE_ISERV/remote-iserv.exe tmp $PORT &
    (>&2 echo "---| remote-iserv should have started on $PORT")
    RISERV_PID="$!"
    ${iserv-proxy}/bin/iserv-proxy $@ 127.0.0.1 "$PORT"
    (>&2 echo "---> killing remote-iserv...")
    kill $RISERV_PID
  '';

  wineIservWrapper = symlinkJoin { name = "iserv-wrapper"; paths = [ (wineIservWrapperScript false) (wineIservWrapperScript true) ]; };

  ################################################################################
  # Build logic (TH support via remote iserv via wine)
  #
  setupBuildFlags = map (opt: "--ghc-option=" + opt) (lib.optionals hostPlatform.isWindows [
    "-fexternal-interpreter"
    "-pgmi" "${wineIservWrapper}/bin/iserv-wrapper"
    # TODO: this should be automatically injected based on the extraLibrary.
    "-L${mingw_w64_pthreads}/lib"
    "-L${mingw_w64_pthreads}/bin"
    "-L${gmp}/lib"
    ]);

  ################################################################################
  # Test logic via wine
  #
  wineTestWrapper = writeScriptBin "test-wrapper" ''
    #!${stdenv.shell}
    set -euo pipefail
    export WINEDLLOVERRIDES="winemac.drv=d"
    export WINEDEBUG=warn-all,fixme-all,-menubuilder,-mscoree,-ole,-secur32,-winediag
    export LC_ALL=en_US.UTF-8
    export WINEPREFIX=$TMP
    Path="''${Path:-}"
    for path in ''${nativeBuildInputs:-}; do
      if [ -d "$path/bin" ]; then
        Path="$Path;$(${wine}/bin/winepath -w $path/bin)";
      fi
    done
    export Path
    ${wine}/bin/wine64 $@
  '';
  testWrapper = lib.optional hostPlatform.isWindows "${wineTestWrapper}/bin/test-wrapper";

  preCheck = lib.optionalString hostPlatform.isWindows ''
    echo "================================================================================"
    echo "RUNNING TESTS for $name via wine64"
    echo "================================================================================"
    echo "Copying extra test libraries ..."
    for p in ${lib.concatStringsSep " "extra-test-libs}; do
      find "$p" -iname '*.dll' -exec cp {} . \;
    done
    # copy all .dlls into the local directory.
    # we ask ghc-pkg for *all* dynamic-library-dirs and then iterate over the unique set
    # to copy over dlls as needed.
    echo "Copying library dependencies..."
    for libdir in $(${hostPlatform.config}-ghc-pkg field "*" dynamic-library-dirs --simple-output|xargs|sed 's/ /\n/g'|sort -u); do
      if [ -d "$libdir" ]; then
        find "$libdir" -iname '*.dll' -exec cp {} . \;
      fi
    done
  '';
  postCheck = lib.optionalString hostPlatform.isWindows ''
    echo "================================================================================"
    echo "END RUNNING TESTS"
    echo "================================================================================"
  '';

in { inherit preCheck testWrapper postCheck setupBuildFlags configureFlags; }
