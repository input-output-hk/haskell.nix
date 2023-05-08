{ stdenv, buildPackages, lib, haskellLib, ghc, ghcForComponent, nonReinstallablePkgs, runCommand, writeText, writeScript, makeConfigFiles }:

{ componentId
, component
, package
, flags
, commonAttrs
, preHaddock
, postHaddock
, pkgconfig
, commonConfigureFlags

, doHaddock
, doHoogle
, hyperlinkSource
, quickjump
, setupHaddockFlags

, needsProfiling
, componentDrv
, configFiles # component config files
}:

let
  doHaddock' = doHaddock
    && (haskellLib.isLibrary componentId)
    && !haskellLib.isCrossHost;

  # the target dir for haddock documentation
  docdir = docoutput: docoutput + "/share/doc/" + componentId.cname;

  packageCfgDir = configFiles.packageCfgDir;

  fullName = "${componentDrv.name}-haddock";

  # These config files are like the one used in the build derivation,
  # but `chooseDrv` will be used to map all the references to libraries
  # to their haddock derivation.
  docsConfigFiles = makeConfigFiles {
    inherit (package) identifier;
    inherit component fullName flags needsProfiling;
    chooseDrv = p: p.haddock;
    inherit (componentDrv) enableDWARF;
  };

  finalConfigureFlags = lib.concatStringsSep " " (
    [ "--prefix=${componentDrv}"
      "${haskellLib.componentTarget componentId}"
      "$(cat $configFiles/configure-flags)"
    ]
    ++ commonConfigureFlags
    ++ lib.optional doHaddock' " --docdir=${docdir "$doc"}");

  shellWrappers = ghcForComponent {
    componentName = fullName;
    configFiles = docsConfigFiles;
    inherit (componentDrv) enableDWARF;
    inherit (component) plugins;
  };

  drv = stdenv.mkDerivation (commonAttrs // {
    name = fullName;

    passthru = {
      # The directory containing the haddock documentation.
      haddockDir = lib.const (if doHaddock' then "${docdir drv.doc}/html" else null);
    };

    # `out` contains the `package.conf.d` files used for building the
    # haddock files.
    # `doc` contains just the haddock output files.
    outputs = ["out" "configFiles" "ghc"]
    ++ lib.optional doHaddock' "doc";

    propagatedBuildInputs = haskellLib.checkUnique "${fullName} propagatedBuildInputs" (
         haskellLib.uniqueWithName (map lib.getDev (builtins.concatLists pkgconfig))
      ++ configFiles.libDeps); # libDeps is already deduplicated

    buildInputs = haskellLib.uniqueWithName (lib.flatten component.libs);

    nativeBuildInputs =
      [ ghc buildPackages.removeReferencesTo ]
      ++ componentDrv.executableToolDepends;

    configurePhase = ''
      mkdir -p $configFiles
      mkdir -p $ghc
      wrappedGhc=$ghc
      ${docsConfigFiles.script}
      ${shellWrappers.script}
      PATH=$wrappedGhc/bin:$PATH
      runHook preConfigure
      echo Configure flags:
      printf "%q " ${finalConfigureFlags}
      echo
      $SETUP_HS configure ${finalConfigureFlags}
      runHook postConfigure
    '';

    buildPhase = ''
      mkdir -p $out
    '' + lib.optionalString doHaddock' ''
      runHook preHaddock

      docdir="${docdir "$doc"}"
      # This mkdir needed for packages like base-noprelude and bytestring-builder
      # (which is also empty when `bytestring >= 0.10.4`)
      mkdir -p "$docdir"

      # If we don't have any source files, no need to run haddock
      [[ -n $(find . -name "*.hs" -o -name "*.lhs") ]] && {
      $SETUP_HS haddock \
        "--html" \
        ${lib.optionalString doHoogle "--hoogle"} \
        ${lib.optionalString hyperlinkSource "--hyperlink-source"} \
        ${lib.optionalString quickjump "--quickjump"} \
        ${lib.concatStringsSep " " setupHaddockFlags}
      }
      runHook postHaddock
    '';

    installPhase =
      let
        target-pkg-and-db = "${ghc.targetPrefix}ghc-pkg -v0 --package-db $out/package.conf.d";
      in ''
        html="dist/doc/html/${package.identifier.name}"

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
          # Copy the .conf files from the build derivation, but replace the `haddock-intefaces:`
          # field with correct location for haddocks (now it is known).  This will insure that
          # the other haddock derivations build to reference this one will have the correct
          # working hyper links.
          pkg=$(basename "$i")
          sed -e "s|haddock-interfaces:.*|haddock-interfaces: $docdir/html/${componentId.cname}.haddock|" -e "s|haddock-html:.*|haddock-html: $docdir/html/|" "$i" > "$pkg"
          ${ghc.targetPrefix}ghc-pkg -v0 --package-db $configFiles/${configFiles.packageCfgDir} -f $out/package.conf.d register "$pkg"
        done

        ln -s ${componentDrv}/exactDep $out/exactDep
        ln -s ${componentDrv}/envDep $out/envDep
      '';
  }
  // haskellLib.optionalHooks {
    inherit preHaddock postHaddock;
  });
in drv
