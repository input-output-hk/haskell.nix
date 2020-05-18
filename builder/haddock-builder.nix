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
      "--docdir=$out"
      "${haskellLib.componentTarget componentId}"
      "$(cat ${docsConfigFiles}/configure-flags)"
      # GHC
      "--with-ghc=${ghc.targetPrefix}ghc"
      "--with-ghc-pkg=${ghc.targetPrefix}ghc-pkg"
      "--with-hsc2hs=${ghc.targetPrefix}hsc2hs"
    ]
    # ] ++ lib.optionals (stdenv.hasCC or (stdenv.cc != null))
    # ( # CC
    #   [ "--with-gcc=${stdenv.cc.targetPrefix}cc"
    #   ] ++
    #   # BINTOOLS
    #   (if stdenv.hostPlatform.isLinux
    #     # use gold as the linker on linux to improve link times
    #     then [
    #       "--with-ld=${stdenv.cc.bintools.targetPrefix}ld.gold"
    #       "--ghc-option=-optl-fuse-ld=gold"
    #       "--ld-option=-fuse-ld=gold"
    #     ] else [
    #       "--with-ld=${stdenv.cc.bintools.targetPrefix}ld"
    #     ]
    #   ) ++ [
    #     "--with-ar=${stdenv.cc.bintools.targetPrefix}ar"
    #     "--with-strip=${stdenv.cc.bintools.targetPrefix}strip"
    #   ]
    # ) ++ [ # other flags
    #   (disableFeature dontStrip "executable-stripping")
    #   (disableFeature dontStrip "library-stripping")
    #   (enableFeature enableLibraryProfiling "library-profiling")
    #   (enableFeature enableExecutableProfiling "executable-profiling")
    #   (enableFeature enableStatic "static")
    #   (enableFeature enableShared "shared")
    # ] ++ lib.optionals (stdenv.hostPlatform.isMusl && (haskellLib.isExecutableType componentId)) [
    #   # These flags will make sure the resulting executable is statically linked.
    #   # If it uses other libraries it may be necessary for to add more
    #   # `--ghc-option=-optl=-L` options to the `configurationFlags` of the
    #   # component.
    #   "--disable-executable-dynamic"
    #   "--ghc-option=-optl=-pthread"
    #   "--ghc-option=-optl=-static"
    #   "--ghc-option=-optl=-L${gmp.override { withStatic = true; }}/lib"
    #   "--ghc-option=-optl=-L${zlib.static}/lib"
    #   "--ghc-option=-optl=-L${ncurses.override { enableStatic = true; }}/lib"
    # ] ++ lib.optional enableSeparateDataOutput "--datadir=$data/share/${ghc.name}"
    #   ++ lib.optional (enableLibraryProfiling || enableExecutableProfiling) "--profiling-detail=${profilingDetail}"
    #   ++ lib.optional stdenv.hostPlatform.isLinux (enableFeature enableDeadCodeElimination "split-sections")
    #   ++ lib.optionals haskellLib.isCrossHost (
    #     map (arg: "--hsc2hs-option=" + arg) (["--cross-compile"] ++ lib.optionals (stdenv.hostPlatform.isWindows) ["--via-asm"])
    #     ++ lib.optional (package.buildType == "Configure") "--configure-option=--host=${stdenv.hostPlatform.config}" )
      ++ configureFlags
      ++ (ghc.extraConfigureFlags or [])
      # ++ lib.optional enableDebugRTS "--ghc-option=-debug"
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

  #   # The directory containing the haddock documentation.
  #   # `null' if no haddock documentation was built.
    haddockDir = "${docdir drv}/html";
  };

  enableParallelBuilding = true;

  # buildInputs = component.libs
  #   ++ frameworks
  #   ++ builtins.concatLists pkgconfig
  #   # Note: This is a hack until we can fix properly. See:
  #   # https://github.com/haskell-gi/haskell-gi/issues/226
  #   ++ lib.optional (lib.strings.hasPrefix "gi-" fullName) gobject-introspection;

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
      docdir="${docdir "$out"}"
      mkdir -p "$docdir"
      cp -R dist/doc/html/ "$docdir"
      exit 1
    '';

  # buildPhase = ''
  #   set -x
  #   runHook preHaddock
  #   # If we don't have any source files, no need to run haddock
  #   [[ -n $(find . -name "*.hs" -o -name "*.lhs") ]] && {

  #   $SETUP_HS --version
  #   $SETUP_HS haddock --help

  #   $SETUP_HS haddock \
  #     "--html" \
  #     ${lib.optionalString doHoogle "--hoogle"} \
  #     ${lib.optionalString hyperlinkSource "--hyperlink-source"} \
  #     ${lib.concatStringsSep " " (setupHaddockFlags ++ setupGhcOptions)}
  #   }
  #   runHook postHaddock
  # '';

  # installPhase = ''
  #   set -x
  #   pwd
  #   ls -l
  #   ls -l dist/
  #   ls -l dist/doc
  #   ls -l dist/doc/html/
  #   ls -l dist/doc/html/${package.identifier.name}
  #   # html="dist/doc/html/${componentId.cname}"
  #   html="dist/doc/html/${package.identifier.name}"

  #   if [ -d "$html" ]; then
  #      # Ensure that libraries are not pulled into the docs closure.
  #      # As an example, the prettified source code of a
  #      # Paths_package module will contain store paths of the library package.
  #      for x in "$html/src/"*.html; do
  #        remove-references-to -t $out $x
  #      done

  #      docdir="${docdir "$out"}"
  #      mkdir -p "$docdir"

  #      cp -R "$html" "$docdir"/html
  #   fi
  #   set +x
  # '';
}
# patches can (if they like) depend on the version and revision of the package.
// lib.optionalAttrs (patches != []) { patches = map (p: if builtins.isFunction p then p { inherit (package.identifier) version; inherit revision; } else p) patches; }
// haskellLib.optionalHooks {
  inherit preUnpack postUnpack preConfigure postConfigure
    preBuild postBuild preHaddock postHaddock;
}
// lib.optionalAttrs (stdenv.buildPlatform.libc == "glibc"){ LOCALE_ARCHIVE = "${buildPackages.glibcLocales}/lib/locale/locale-archive"; }
))

