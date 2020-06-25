{ stdenv, buildPackages, lib, haskellLib, ghc, ghcForComponent, nonReinstallablePkgs, runCommand, writeText, writeScript, makeConfigFiles }:

{ componentId
, component
, package
, flags
, revision
, commonAttrs
, configureFlags

, doHoogle
, hyperlinkSource
, setupHaddockFlags
, setupGhcOptions

, needsProfiling
, componentDrv
, configFiles # component config files
}:

let
  # the target dir for haddock documentation
  docdir = docoutput: docoutput + "/share/doc/" + componentId.cname;

  packageCfgDir = configFiles.packageCfgDir;

  fullName = "${componentDrv.name}-haddock";

  docsConfigFiles = makeConfigFiles {
    inherit (package) identifier;
    inherit component fullName flags needsProfiling;
    chooseDrv = p: p.haddock;
  };

  finalConfigureFlags = lib.concatStringsSep " " (
    [ "--prefix=${componentDrv}"
      "--docdir=${docdir "$doc"}"
      "${haskellLib.componentTarget componentId}"
      "$(cat ${docsConfigFiles}/configure-flags)"
      # GHC
      "--with-ghc=${ghc.targetPrefix}ghc"
      "--with-ghc-pkg=${ghc.targetPrefix}ghc-pkg"
      "--with-hsc2hs=${ghc.targetPrefix}hsc2hs"
    ]
      ++ configureFlags
      ++ (ghc.extraConfigureFlags or [])
    );

  shellWrappers = ghcForComponent {
    componentName = fullName;
    configFiles = docsConfigFiles;
  };

in stdenv.lib.fix (drv: stdenv.mkDerivation (commonAttrs // {
  name = fullName;

  passthru = {
    configFiles = docsConfigFiles;

    # The directory containing the haddock documentation.
    haddockDir = "${docdir drv.doc}/html";
  };

  # `out` contains the `package.conf.d` files used for building the
  # haddock files.
  # `doc` contains just the haddock output files.
  outputs = ["out" "doc"];

  nativeBuildInputs =
    [ shellWrappers buildPackages.removeReferencesTo ]
    ++ componentDrv.executableToolDepends;

  configurePhase = ''
    echo Configure flags:
    printf "%q " ${finalConfigureFlags}
    echo
    $SETUP_HS configure ${finalConfigureFlags}
  '';

  buildPhase = ''
    runHook preHaddock
    # If we don't have any source files, no need to run haddock
    [[ -n $(find . -name "*.hs" -o -name "*.lhs") ]] && {

    $SETUP_HS haddock \
      "--html" \
      ${lib.optionalString doHoogle "--hoogle"} \
      ${lib.optionalString hyperlinkSource "--hyperlink-source"} \
      ${lib.concatStringsSep " " (setupHaddockFlags ++ setupGhcOptions)}
    }
    runHook postHaddock
  '';

  installPhase =
    let
      target-pkg-and-db = "${ghc.targetPrefix}ghc-pkg -v0 --package-db $out/package.conf.d";
    in ''
      html="dist/doc/html/${package.identifier.name}"

      ls $html

      if [ -d "$html" ]; then
         # Ensure that libraries are not pulled into the docs closure.
         # As an example, the prettified source code of a
         # Paths_package module will contain store paths of the library package.
         for x in "$html/src/"*.html; do
           remove-references-to -t $out $x
           remove-references-to -t ${componentDrv} $x
         done

         docdir="${docdir "$doc"}"
         mkdir -p "$docdir"

         cp -R "$html" "$docdir"/html
      fi

      ${ghc.targetPrefix}ghc-pkg -v0 init $out/package.conf.d

      for i in "${componentDrv}/package.conf.d"/*.conf; do
        pkg=$(basename "$i")
        sed -e "s|haddock-interfaces:.*|haddock-interfaces: $docdir/html/${componentId.cname}.haddock|" -e "s|haddock-html:.*|haddock-html: $docdir/html/|" "$i" > "$pkg"
        ${ghc.targetPrefix}ghc-pkg -v0 --package-db ${docsConfigFiles}/${configFiles.packageCfgDir} -f $out/package.conf.d register "$pkg"
      done

      cp -Rv ${componentDrv}/exactDep $out/exactDep
      cp -Rv ${componentDrv}/envDep $out/envDep

      set +x
    '';
}
))

