{ stdenv, buildPackages, ghc, lib, pkgconfig, gobject-introspection ? null, haskellLib, makeConfigFiles, ghcForComponent, hsPkgs, runCommand, libffi, gmp }:

{ componentId
, component
, package
, name
, setup
, src
, flags
, revision
, cabalFile
, cabal-generator
, patches ? []

, preUnpack ? component.preUnpack, postUnpack ? component.postUnpack
, preConfigure ? component.preConfigure, postConfigure ? component.postConfigure
, preBuild ? component.preBuild , postBuild ? component.postBuild
, preCheck ? component.preCheck , postCheck ? component.postCheck
, preInstall ? component.preInstall , postInstall ? component.postInstall
, preHaddock ? component.preHaddock , postHaddock ? component.postHaddock
, shellHook ? ""

, dontPatchELF ? component.dontPatchELF
, dontStrip ? component.dontStrip

, enableStatic ? component.enableStatic
, enableShared ? component.enableShared && stdenv.buildPlatform == stdenv.hostPlatform
, enableDeadCodeElimination ? component.enableDeadCodeElimination

# Options for Haddock generation
, doHaddock ? component.doHaddock  # Enable haddock and hoogle generation
, doHoogle ? component.doHoogle # Also build a hoogle index
, hyperlinkSource ? component.doHyperlinkSource # Link documentation to the source code
, keepSource ? component.keepSource  # Build from `source` output in the store then delete `dist`

# Profiling
, enableLibraryProfiling ? component.enableLibraryProfiling
, enableExecutableProfiling ? component.enableExecutableProfiling
, profilingDetail ? component.profilingDetail

# Data
, enableSeparateDataOutput ? component.enableSeparateDataOutput
}:

let
  # TODO fix cabal wildcard support so hpack wildcards can be mapped to cabal wildcards
  cleanSrc = if cabal-generator == "hpack" && !(package.cleanHpack or false)
    then builtins.trace ("Cleaning component source not supported for hpack package: " + name) src
    else haskellLib.cleanCabalComponent package component src;

  fullName = if haskellLib.isAll componentId
    then "${name}-all"
    else "${name}-${componentId.ctype}-${componentId.cname}";

  configFiles = makeConfigFiles {
    inherit (package) identifier;
    inherit component fullName flags;
  };

  enableFeature = enable: feature:
    (if enable then "--enable-" else "--disable-") + feature;

  disableFeature = disable: enableFeature (!disable);

  finalConfigureFlags = lib.concatStringsSep " " (
    [ "--prefix=$out"
      "${haskellLib.componentTarget componentId}"
      "$(cat ${configFiles}/configure-flags)"
      # GHC
      "--with-ghc=${ghc.targetPrefix}ghc"
      "--with-ghc-pkg=${ghc.targetPrefix}ghc-pkg"
      "--with-hsc2hs=${ghc.targetPrefix}hsc2hs"
    ] ++ lib.optionals (stdenv.cc != null)
    ( # CC
      [ "--with-gcc=${stdenv.cc.targetPrefix}cc"
      ] ++
      # BINTOOLS
      (if stdenv.hostPlatform.isLinux
        # use gold as the linker on linux to improve link times
        then [
          "--with-ld=${stdenv.cc.bintools.targetPrefix}ld.gold"
          "--ghc-option=-optl-fuse-ld=gold"
          "--ld-option=-fuse-ld=gold"
        ] else [
          "--with-ld=${stdenv.cc.bintools.targetPrefix}ld"
        ]
      ) ++ [
        "--with-ar=${stdenv.cc.bintools.targetPrefix}ar"
        "--with-strip=${stdenv.cc.bintools.targetPrefix}strip"
      ]
    ) ++ [ # other flags
      (disableFeature dontStrip "executable-stripping")
      (disableFeature dontStrip "library-stripping")
      (enableFeature enableLibraryProfiling "library-profiling")
      (enableFeature enableExecutableProfiling "executable-profiling")
      (enableFeature enableStatic "static")
      (enableFeature enableShared "shared")
    ] ++ lib.optional enableSeparateDataOutput "--datadir=$data/share/${ghc.name}"
      ++ lib.optional doHaddock' "--docdir=${docdir "$doc"}"
      ++ lib.optional (enableLibraryProfiling || enableExecutableProfiling) "--profiling-detail=${profilingDetail}"
      ++ lib.optional stdenv.hostPlatform.isLinux (enableFeature enableDeadCodeElimination "split-sections")
      ++ lib.optionals (
                stdenv.hostPlatform != stdenv.buildPlatform
            # When building for musl on a linux system we do not need to use the `--cross-compile` flag
            && !(stdenv.buildPlatform.isLinux && stdenv.hostPlatform.isMusl)) (
        map (arg: "--hsc2hs-option=" + arg) (["--cross-compile"] ++ lib.optionals (stdenv.hostPlatform.isWindows) ["--via-asm"])
        ++ lib.optional (package.buildType == "Configure") "--configure-option=--host=${stdenv.hostPlatform.config}" )
      ++ component.configureFlags
      ++ (ghc.extraConfigureFlags or [])
    );

  setupGhcOptions = lib.optional (package.ghcOptions != null) '' --ghc-options="${package.ghcOptions}"'';

  executableToolDepends =
    (lib.concatMap (c: if c.isHaskell or false
      then builtins.attrValues (c.components.exes or {})
      else [c]) component.build-tools) ++
    lib.optional (component.pkgconfig != []) pkgconfig;

  # Unfortunately, we need to wrap ghc commands for cabal builds to
  # work in the nix-shell. See ../doc/removing-with-package-wrapper.md.
  shellWrappers = ghcForComponent {
    componentName = fullName;
    inherit configFiles;
  };

  # In order to let shell hooks make package-specific things like Hoogle databases
  shellHookApplied = if builtins.isString shellHook then shellHook else
                     if builtins.isFunction shellHook then shellHook { inherit package shellWrappers; }
                     else abort "shellHook should be a string or a function";

  # the target dir for haddock documentation
  docdir = docoutput: docoutput + "/share/doc/" + componentId.cname;

  doHaddock' = doHaddock
    && (haskellLib.isLibrary componentId);

  exeExt = lib.optionalString stdenv.hostPlatform.isWindows ".exe";
  exeName = componentId.cname + exeExt;
  testExecutable = "dist/build/${componentId.cname}/${exeName}";

in stdenv.lib.fix (drv:

stdenv.mkDerivation ({
  name = "${ghc.targetPrefix}${fullName}";

  src = cleanSrc;

  doCheck = false;
  doCrossCheck = false;

  inherit dontPatchELF dontStrip;

  passthru = {
    inherit (package) identifier;
    config = component;
    inherit configFiles executableToolDepends cleanSrc exeName;
    env = shellWrappers;

    # The directory containing the haddock documentation.
    # `null' if no haddock documentation was built.
    haddockDir = if doHaddock' then "${docdir drv.doc}/html" else null;
  };

  meta = {
    homepage = package.homepage;
    description = package.synopsis;
    license =
      let
        license-map = import ../lib/cabal-licenses.nix lib;
      in license-map.${package.license} or
        (builtins.trace "WARNING: license \"${package.license}\" not found" license-map.LicenseRef-OtherLicense);
    platforms = if component.platforms == null then stdenv.lib.platforms.all else component.platforms;
  };

  LANG = "en_US.UTF-8";         # GHC needs the locale configured during the Haddock phase.
  LC_ALL = "en_US.UTF-8";

  enableParallelBuilding = true;

  buildInputs = component.libs
    ++ component.frameworks
    ++ builtins.concatLists component.pkgconfig
    # Note: This is a hack until we can fix properly. See:
    # https://github.com/haskell-gi/haskell-gi/issues/226
    ++ lib.optional (lib.strings.hasPrefix "gi-" fullName) gobject-introspection;

  nativeBuildInputs =
    [shellWrappers buildPackages.removeReferencesTo]
    ++ executableToolDepends;

  SETUP_HS = setup + /bin/Setup;

  outputs = ["out" ]
    ++ (lib.optional enableSeparateDataOutput "data")
    ++ (lib.optional doHaddock' "doc")
    ++ (lib.optional keepSource "source");

  # Phases
  preInstallPhases = lib.optional doHaddock' "haddockPhase";

  prePatch = if (cabalFile != null)
     then ''cat ${cabalFile} > ${package.identifier.name}.cabal''
     else lib.optionalString (cabal-generator == "hpack") ''
       ${buildPackages.haskell-nix.haskellPackages.hpack.components.exes.hpack}/bin/hpack
     '';

  configurePhase =
    (lib.optionalString keepSource ''
      cp -r . $source
      cd $source
      chmod -R +w .
    '') + ''
    runHook preConfigure
    echo Configure flags:
    printf "%q " ${finalConfigureFlags}
    echo
    $SETUP_HS configure ${finalConfigureFlags}
    runHook postConfigure
  '';

  buildPhase = ''
    runHook preBuild
    # https://gitlab.haskell.org/ghc/ghc/issues/9221
    $SETUP_HS build ${haskellLib.componentTarget componentId} -j$(($NIX_BUILD_CORES > 4 ? 4 : $NIX_BUILD_CORES)) ${lib.concatStringsSep " " (component.setupBuildFlags ++ setupGhcOptions)}
    runHook postBuild
  '';

  checkPhase = "notice: Tests are only executed by building the .run sub-derivation of this component.";

  haddockPhase = ''
    runHook preHaddock
    # If we don't have any source files, no need to run haddock
    [[ -n $(find . -name "*.hs" -o -name "*.lhs") ]] && {
    docdir="${docdir "$doc"}"
    mkdir -p "$docdir"

    $SETUP_HS haddock \
      "--html" \
      ${lib.optionalString doHoogle "--hoogle"} \
      ${lib.optionalString hyperlinkSource "--hyperlink-source"} \
      ${lib.concatStringsSep " " (component.setupHaddockFlags ++ setupGhcOptions)}

    html="dist/doc/html/${componentId.cname}"

    if [ -d "$html" ]; then
       # Ensure that libraries are not pulled into the docs closure.
       # As an example, the prettified source code of a
       # Paths_package module will contain store paths of the library package.
       for x in "$html/src/"*.html; do
         remove-references-to -t $out $x
       done

       cp -R "$html" "$docdir"/html
    fi
    }
    runHook postHaddock
  '';

  # Note: Cabal does *not* copy test executables during the `install` phase.
  #
  # Note 2: if a package contains multiple libs (lib + sublibs) SETUP register will generate a
  #         folder isntead of a file for the configuration.  Therfore if the result is a folder,
  #         we need to register each and every element of that folder.
  #
  # Note 3: if a package has no libs SETUP will not generate anything.  This can
  #         happen when building the `all` component of a package.
  installPhase = let
      target-pkg-and-db = "${ghc.targetPrefix}ghc-pkg -v0 --package-db $out/package.conf.d";
    in ''
    runHook preInstall
    $SETUP_HS copy ${lib.concatStringsSep " " component.setupInstallFlags}
    ${lib.optionalString (haskellLib.isLibrary componentId || haskellLib.isAll componentId) ''
      $SETUP_HS register --gen-pkg-config=${name}.conf
      ${ghc.targetPrefix}ghc-pkg -v0 init $out/package.conf.d
      if [ -d "${name}.conf" ]; then
        for pkg in ${name}.conf/*; do
          ${ghc.targetPrefix}ghc-pkg -v0 --package-db ${configFiles}/${configFiles.packageCfgDir} -f $out/package.conf.d register "$pkg"
        done
      elif [ -e "${name}.conf" ]; then
        ${ghc.targetPrefix}ghc-pkg -v0 --package-db ${configFiles}/${configFiles.packageCfgDir} -f $out/package.conf.d register ${name}.conf
      fi

      mkdir -p $out/exactDep
      touch $out/exactDep/configure-flags
      touch $out/exactDep/cabal.config

      if id=$(${target-pkg-and-db} field ${package.identifier.name} id --simple-output); then
        echo "--dependency=${package.identifier.name}=$id" >> $out/exactDep/configure-flags
      elif id=$(${target-pkg-and-db} field "z-${package.identifier.name}-z-*" id --simple-output); then
        name=$(${target-pkg-and-db} field "z-${package.identifier.name}-z-*" name --simple-output)
        # so we are dealing with a sublib. As we build sublibs separately, the above
        # query should be safe.
        echo "--dependency=''${name#z-${package.identifier.name}-z-}=$id" >> $out/exactDep/configure-flags
      fi
      if ver=$(${target-pkg-and-db} field ${package.identifier.name} version --simple-output); then
        echo "constraint: ${package.identifier.name} == $ver" >> $out/exactDep/cabal.config
        echo "constraint: ${package.identifier.name} installed" >> $out/exactDep/cabal.config
      fi

      touch $out/envDep
      if id=$(${target-pkg-and-db} field ${package.identifier.name} id --simple-output); then
        echo "package-id $id" >> $out/envDep
      fi
    ''}
    ${(lib.optionalString (haskellLib.isTest componentId || haskellLib.isBenchmark componentId || haskellLib.isAll componentId) ''
      mkdir -p $out/bin
      if [ -f ${testExecutable} ]; then
        cp ${testExecutable} $out/bin/
      fi
    '')
    # In case `setup copy` did not creat this
    + (lib.optionalString enableSeparateDataOutput "mkdir -p $data")
    + (lib.optionalString (stdenv.hostPlatform.isWindows && (haskellLib.mayHaveExecutable componentId)) ''
      echo "Symlink libffi and gmp .dlls ..."
      for p in ${lib.concatStringsSep " " [ libffi gmp ]}; do
        find "$p" -iname '*.dll' -exec ln -s {} $out/bin \;
      done
      # symlink all .dlls into the local directory.
      # we ask ghc-pkg for *all* dynamic-library-dirs and then iterate over the unique set
      # to symlink over dlls as needed.
      echo "Symlink library dependencies..."
      for libdir in $(x86_64-pc-mingw32-ghc-pkg --package-db=$packageConfDir field "*" dynamic-library-dirs --simple-output|xargs|sed 's/ /\n/g'|sort -u); do
        if [ -d "$libdir" ]; then
          find "$libdir" -iname '*.dll' -exec ln -s {} $out/bin \;
        fi
      done
    '')
    }
    runHook postInstall
  '' + (lib.optionalString keepSource ''
    rm -rf dist
  '');

  shellHook = ''
    export PATH="${shellWrappers}/bin:$PATH"
    ${shellHookApplied}
  '';
}
# patches can (if they like) depend on the version and revision of the package.
// lib.optionalAttrs (patches != []) { patches = map (p: if builtins.isFunction p then p { inherit (package.identifier) version; inherit revision; } else p) patches; }
// haskellLib.optionalHooks {
  inherit preUnpack postUnpack preConfigure postConfigure
    preBuild postBuild
    preInstall postInstall preHaddock postHaddock;
}
// lib.optionalAttrs (stdenv.buildPlatform.libc == "glibc"){ LOCALE_ARCHIVE = "${buildPackages.glibcLocales}/lib/locale/locale-archive"; }
))
