{ stdenv, buildPackages, lib, haskellLib, ghc, ghcForComponent, nonReinstallablePkgs, runCommand, writeText, writeScript, makeConfigFiles }:

{ componentId
, component
, package
, name
, setup
, src
, flags
, revision
, patches

, preUnpack
, postUnpack
, configureFlags
, preConfigure
, postConfigure
, preBuild
, postBuild
, preHaddock
, postHaddock
, setupInstallFlags

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

  fullName = if haskellLib.isAll componentId
    then "${name}-docs-all"
    else "${name}-${componentId.ctype}-${componentId.cname}-docs";

  docsConfigFiles = makeConfigFiles {
    inherit (package) identifier;
    inherit component fullName flags needsProfiling;
    chooseDrv = p: p.docs;
  };

  # let
  #   # depDocConfig = map (x: x.configFiles) (haskellLib.flatLibDepends component);
  # in runCommand "${ghc.targetPrefix}${fullName}-docs-config" {
  #   nativeBuildInputs = [ghc];
  # } (''
  #   mkdir -p $out

  #   for i in cabal.config configure-flags ghc-environment; do
  #     substitute "${configFiles}/$i" "$out/$i"  --replace "${configFiles}" "$out"
  #   done

  #   mkdir -p $out/${packageCfgDir}

  #   # Copy over the nonReinstallablePkgs from the global package db.
  #   ${lib.concatMapStringsSep "\n" (p: ''
  #     find ${ghc}/lib/${ghc.name}/package.conf.d -name '${p}*.conf' -exec cp -f {} $out/${packageCfgDir} \;
  #   '') nonReinstallablePkgs}

  # '');

  finalConfigureFlags = lib.concatStringsSep " " (
    [ "--prefix=${componentDrv}"
      "--docdir=${docdir "$out"}"
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

in stdenv.lib.fix (drv: stdenv.mkDerivation ({
  name = "${ghc.targetPrefix}${fullName}";
  inherit src;

  LANG = "en_US.UTF-8";         # GHC needs the locale configured during the Haddock phase.
  LC_ALL = "en_US.UTF-8";

  passthru = {
    configFiles = docsConfigFiles;

    # The directory containing the haddock documentation.
    # `null' if no haddock documentation was built.
    haddockDir = "${docdir drv}/html";
  };

  enableParallelBuilding = true;

  nativeBuildInputs =
    [ shellWrappers buildPackages.removeReferencesTo ]
    ++ componentDrv.executableToolDepends;

  SETUP_HS = setup + /bin/Setup;

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
      set -x
      html="dist/doc/html/${package.identifier.name}"

      ls $html

      if [ -d "$html" ]; then
         # Ensure that libraries are not pulled into the docs closure.
         # As an example, the prettified source code of a
         # Paths_package module will contain store paths of the library package.
         for x in "$html/src/"*.html; do
           remove-references-to -t $out $x
         done

         docdir="${docdir "$out"}"
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
# patches can (if they like) depend on the version and revision of the package.
// lib.optionalAttrs (patches != []) { patches = map (p: if builtins.isFunction p then p { inherit (package.identifier) version; inherit revision; } else p) patches; }
// haskellLib.optionalHooks {
  inherit preUnpack postUnpack preConfigure postConfigure
    preBuild postBuild preHaddock postHaddock;
}
// lib.optionalAttrs (stdenv.buildPlatform.libc == "glibc"){ LOCALE_ARCHIVE = "${buildPackages.glibcLocales}/lib/locale/locale-archive"; }
))

