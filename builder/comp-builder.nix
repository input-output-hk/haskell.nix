{ pkgs, stdenv, buildPackages, ghc, lib, gobject-introspection ? null, haskellLib, makeConfigFiles, haddockBuilder, ghcForComponent, hsPkgs, compiler, runCommand, libffi, gmp, zlib, ncurses, nodejs }@defaults:
lib.makeOverridable (
let self =
{ componentId
, component
, package
, name # This is the package name
, setup
, src
, flags
, revision
, cabalFile
, cabal-generator
, patches ? []

, preUnpack ? component.preUnpack, postUnpack ? component.postUnpack
, configureFlags ? component.configureFlags
, preConfigure ? component.preConfigure, postConfigure ? component.postConfigure
, setupBuildFlags ? component.setupBuildFlags
, preBuild ? component.preBuild , postBuild ? component.postBuild
, preCheck ? component.preCheck , postCheck ? component.postCheck
, setupInstallFlags ? component.setupInstallFlags
, preInstall ? component.preInstall , postInstall ? component.postInstall
, preHaddock ? component.preHaddock , postHaddock ? component.postHaddock
, shellHook ? ""
, configureAllComponents ? component.configureAllComponents ||
    # When set, configure all the components in the package
    # (not just the one we are building).
    # Enable for tests in packages that use cabal-doctest.
    ( haskellLib.isTest componentId &&
      lib.any (x: x.identifier.name or "" == "cabal-doctest") package.setup-depends
    )
, allComponent # Used when `configureAllComponents` is set to get a suitable configuration.

, build-tools ? component.build-tools
, pkgconfig ? component.pkgconfig
, platforms ? component.platforms
, frameworks ? component.frameworks

, dontPatchELF ? component.dontPatchELF
, dontStrip ? component.dontStrip
, hardeningDisable ? component.hardeningDisable

, enableStatic ? component.enableStatic
, enableShared ? ghc.enableShared && component.enableShared && !haskellLib.isCrossHost
               # on x86 we'll use shared libraries, even with musl m(
               # ghc's internal linker seems to be broken on x86.
               && !(stdenv.hostPlatform.isMusl && !stdenv.hostPlatform.isx86)
, enableDeadCodeElimination ? component.enableDeadCodeElimination
, writeHieFiles ? component.writeHieFiles

, ghcOptions ? component.ghcOptions
, contentAddressed ? component.contentAddressed

# Options for Haddock generation
, doHaddock ? component.doHaddock  # Enable haddock and hoogle generation
, doHoogle ? component.doHoogle # Also build a hoogle index
, hyperlinkSource ? component.doHyperlinkSource # Link documentation to the source code
, quickjump ? component.doQuickjump # Generate an index for interactive documentation navigation
, keepSource ? component.keepSource || configureAllComponents # Build from `source` output in the store then delete `dist`
, setupHaddockFlags ? component.setupHaddockFlags

# Profiling
, enableLibraryProfiling ? component.enableLibraryProfiling
, enableProfiling ? component.enableProfiling
, profilingDetail ? component.profilingDetail

# Coverage
, doCoverage ? component.doCoverage

# Data
, enableSeparateDataOutput ? component.enableSeparateDataOutput

# Prelinked ghci libraries; will make iserv faster; especially for static builds.
, enableLibraryForGhci ? true

# Debug
, enableDebugRTS ? false
, enableDWARF ? false

# This will only work with a custom TSan way enabled custom compiler
, enableTSanRTS ? false

# LLVM
, useLLVM ? ghc.useLLVM
, smallAddressSpace ? false

}@drvArgs:

let
  componentForSetup =
    if configureAllComponents
      then allComponent
      else component;

  # Ignore attempts to include DWARF info when it is not possible
  enableDWARF = drvArgs.enableDWARF or false
    && stdenv.hostPlatform.isLinux
    && !haskellLib.isCrossHost
    && !stdenv.hostPlatform.isMusl
    && builtins.compareVersions defaults.ghc.version "8.10.2" >= 0;

  ghc = (if enableDWARF then (x: x.dwarf) else (x: x)) (
        (if smallAddressSpace then (x: x.smallAddressSpace) else (x: x)) defaults.ghc);
  setup = (if enableDWARF then (x: x.dwarf) else (x: x)) (
        (if smallAddressSpace then (x: x.smallAddressSpace) else (x: x)) drvArgs.setup);

  # TODO fix cabal wildcard support so hpack wildcards can be mapped to cabal wildcards
  canCleanSource = !(cabal-generator == "hpack" && !(package.cleanHpack or false));
  # In order to support relative references to other packages we need to use
  # the `origSrc` diretory as the root `src` for the derivation.
  # We use `rootAndSubDir` here to split the cleaned source into a `cleanSrc.root`
  # path (that respects the filtering) and a `cleanSrc.subDir` that
  # is the sub directory in that root path that contains the package.
  # `cleanSrc.subDir` is used in `prePatch` and `lib/cover.nix`.
  cleanSrc = haskellLib.rootAndSubDir (if canCleanSource
    then haskellLib.cleanCabalComponent package componentForSetup "${componentId.ctype}-${componentId.cname}" src
    else
      # We can clean out the siblings though to at least avoid changes to other packages
      # from triggering a rebuild of this one.
      # Passing `subDir` but not `includeSiblings = true;` will exclude anything not
      # in the `subDir`.
      if src ? origSrc && src ? filter && src.origSubDir or "" != ""
        then haskellLib.cleanSourceWith {
          name = src.name or "source";
          src = src.origSrc;
          subDir = lib.removePrefix "/" src.origSubDir;
          inherit (src) filter;
        }
        else src);

  nameOnly = "${package.identifier.name}-${componentId.ctype}-${componentId.cname}";

  fullName = "${nameOnly}-${package.identifier.version}";

  needsProfiling = enableProfiling || enableLibraryProfiling;

  configFiles = makeConfigFiles {
    component = componentForSetup;
    inherit (package) identifier;
    inherit fullName flags needsProfiling enableDWARF;
  };

  enableFeature = enable: feature:
    (if enable then "--enable-" else "--disable-") + feature;

  disableFeature = disable: enableFeature (!disable);

  finalConfigureFlags = lib.concatStringsSep " " (
    [ "--prefix=$out"
    ] ++
    # We don't specify this in 'commonConfigureFlags', as these are also
    # used by haddock-builder.nix. If we do specify these options in
    # commonConfigureFlags, then the haddock-builder will fail, because it
    # sets its own outputs which *don't* include $hie
    lib.optionals writeHieFiles [
      "--ghc-option=-fwrite-ide-info" "--ghc-option=-hiedir$hie"
    ] ++
    (
      # If configureAllComponents is set we should not specify the component
      # and Setup will attempt to configure them all.
      if configureAllComponents
        then ["--enable-tests" "--enable-benchmarks"]
        else ["${haskellLib.componentTarget componentId}"]
    ) ++ [ "$(cat ${configFiles}/configure-flags)"
    ] ++ commonConfigureFlags);

  # From nixpkgs 20.09, the pkg-config exe has a prefix matching the ghc one
  pkgConfigHasPrefix = builtins.compareVersions lib.version "20.09pre" >= 0;

  commonConfigureFlags = ([
      # GHC
      "--with-ghc=${ghc.targetPrefix}ghc"
      "--with-ghc-pkg=${ghc.targetPrefix}ghc-pkg"
      "--with-hsc2hs=${ghc.targetPrefix}hsc2hs"
    ] ++ lib.optional (pkgConfigHasPrefix && pkgconfig != [])
      "--with-pkg-config=${ghc.targetPrefix}pkg-config"
      ++ lib.optionals (stdenv.hasCC or (stdenv.cc != null))
    ( # CC
      [ "--with-gcc=${stdenv.cc.targetPrefix}cc"
      ] ++
      # BINTOOLS
      (if stdenv.hostPlatform.isLinux && !stdenv.hostPlatform.isAndroid # might be better check to see if cc is clang/llvm?
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
      (enableFeature enableProfiling "profiling")
      (enableFeature enableStatic "static")
      (enableFeature enableShared "shared")
      (enableFeature doCoverage "coverage")
      (enableFeature enableLibraryForGhci "library-for-ghci")
    ] ++ lib.optionals (stdenv.hostPlatform.isMusl && (haskellLib.isExecutableType componentId)) [
      # These flags will make sure the resulting executable is statically linked.
      # If it uses other libraries it may be necessary for to add more
      # `--ghc-option=-optl=-L` options to the `configureFlags` of the
      # component.
      "--disable-executable-dynamic"
      "--ghc-option=-optl=-pthread"
      "--ghc-option=-optl=-static"
    ] ++ lib.optional enableSeparateDataOutput "--datadir=$data/share/${ghc.name}"
      ++ lib.optional (enableLibraryProfiling || enableProfiling) "--profiling-detail=${profilingDetail}"
      ++ lib.optional stdenv.hostPlatform.isLinux (enableFeature enableDeadCodeElimination "split-sections")
      ++ lib.optionals haskellLib.isCrossHost (
        map (arg: "--hsc2hs-option=" + arg) (["--cross-compile"] ++ lib.optionals (stdenv.hostPlatform.isWindows) ["--via-asm"])
        ++ lib.optional (package.buildType == "Configure") "--configure-option=--host=${stdenv.hostPlatform.config}" )
      ++ configureFlags
      ++ (ghc.extraConfigureFlags or [])
      ++ lib.optional enableDebugRTS "--ghc-option=-debug"
      ++ lib.optional enableTSanRTS "--ghc-option=-tsan"
      ++ lib.optional enableDWARF "--ghc-option=-g3"
      ++ lib.optionals useLLVM [
        "--ghc-option=-fPIC" "--gcc-option=-fPIC"
        ]
      ++ map (o: ''--ghc${lib.optionalString (stdenv.hostPlatform.isGhcjs) "js"}-options="${o}"'') ghcOptions
      ++ lib.optional (
        # GHC 9.2 cross compiler built with older versions of GHC seem to have problems
        # with unique conters.  Perhaps because the name changed for the counters.
        # TODO This work around to use `-j1` should be removed once we are able to build 9.2 with 9.2.
        haskellLib.isCrossHost
          && builtins.compareVersions defaults.ghc.version "9.2.1" >= 0
          && builtins.compareVersions defaults.ghc.version "9.3" < 0)
        "--ghc-options=-j1"
    );

  executableToolDepends =
    (lib.concatMap (c: if c.isHaskell or false
      then builtins.attrValues (c.components.exes or {})
      else [c]) build-tools) ++
    lib.optional (pkgconfig != []) buildPackages.cabalPkgConfigWrapper;

  # Unfortunately, we need to wrap ghc commands for cabal builds to
  # work in the nix-shell. See ../doc/removing-with-package-wrapper.md.
  shellWrappers = ghcForComponent {
    componentName = fullName;
    inherit configFiles enableDWARF;
    inherit (component) plugins;
  };

  # In order to let shell hooks make package-specific things like Hoogle databases
  shellHookApplied = if builtins.isString shellHook then shellHook else
                     if builtins.isFunction shellHook then shellHook { inherit package shellWrappers; }
                     else abort "shellHook should be a string or a function";

  exeExt = if stdenv.hostPlatform.isGhcjs then ".jsexe/all.js" else
    stdenv.hostPlatform.extensions.executable;
  exeName = componentId.cname + exeExt;
  testExecutable = "dist/build/${componentId.cname}/${exeName}";

  # Attributes that are common to both the build and haddock derivations
  commonAttrs = {
      src = cleanSrc.root;

      LANG = "en_US.UTF-8";         # GHC needs the locale configured during the Haddock phase.
      LC_ALL = "en_US.UTF-8";

      enableParallelBuilding = true;

      SETUP_HS = setup + /bin/Setup;

      prePatch =
        # If the package is in a sub directory `cd` there first.
        # In some cases the `cleanSrc.subDir` will be empty and the `.cabal`
        # file will be in the root of `src` (`cleanSrc.root`).  This
        # will happen when:
        #   * the .cabal file is in the projects `src.origSrc or src`
        #   * the package src was overridden with a value that does not
        #     include an `origSubDir`
        (lib.optionalString (cleanSrc.subDir != "") ''
            cd ${lib.removePrefix "/" cleanSrc.subDir}
          ''
        ) +
        (if cabalFile != null
          then ''cat ${cabalFile} > ${package.identifier.name}.cabal''
          else
            # When building hpack package we use the internal nix-tools
            # (compiled with a fixed GHC version)
            lib.optionalString (cabal-generator == "hpack") ''
              ${buildPackages.haskell-nix.internal-nix-tools}/bin/hpack
            ''
        );
    }
    # patches can (if they like) depend on the version and revision of the package.
    // lib.optionalAttrs (patches != []) {
      patches = map (p:
        if builtins.isFunction p
          then p { inherit (package.identifier) version; inherit revision; }
          else p
        ) patches;
    }
    // haskellLib.optionalHooks {
      # These are hooks are needed to set up the source for building and running haddock
      inherit preUnpack postUnpack preConfigure postConfigure;
    }
    // lib.optionalAttrs (stdenv.buildPlatform.libc == "glibc") {
      LOCALE_ARCHIVE = "${buildPackages.glibcLocales}/lib/locale/locale-archive";
    };

  haddock = haddockBuilder {
    inherit componentId component package flags commonConfigureFlags
      commonAttrs revision doHaddock
      doHoogle hyperlinkSource quickjump setupHaddockFlags
      needsProfiling configFiles preHaddock postHaddock pkgconfig;

    componentDrv = drv;
  };

  contentAddressedAttrs = lib.optionalAttrs contentAddressed {
    __contentAddressed = true;
    outputHashMode = "recursive";
    outputHashAlgo = "sha256";
  };
                    
  drv = stdenv.mkDerivation (commonAttrs // contentAddressedAttrs // {
    pname = nameOnly;
    inherit (package.identifier) version;

    doCheck = false;
    doCrossCheck = false;

    inherit dontPatchELF dontStrip;

    passthru = {
      inherit (package) identifier;
      config = component;
      srcSubDir = cleanSrc.subDir;
      srcSubDirPath = cleanSrc.root + cleanSrc.subDir;
      inherit configFiles executableToolDepends exeName enableDWARF;
      exePath = drv + "/bin/${exeName}";
      env = shellWrappers;
      profiled = self (drvArgs // { enableLibraryProfiling = true; });
      dwarf = self (drvArgs // { enableDWARF = true; });
    } // lib.optionalAttrs (haskellLib.isLibrary componentId) ({
        inherit haddock;
        inherit (haddock) haddockDir; # This is null if `doHaddock = false`
      } // lib.optionalAttrs (haddock ? doc) {
        # `doHaddock = false` turns the doc of haddock output off
        inherit (haddock) doc;
      }
    );

    meta = {
      homepage = package.homepage or "";
      description = package.synopsis or "";
      license = haskellLib.cabalToNixpkgsLicense package.license;
      platforms = if platforms == null then lib.platforms.all else platforms;
    } // lib.optionalAttrs (haskellLib.isExecutableType componentId) {
      # Set main executable name for executable components, so that `nix run` in
      # nix flakes will work correctly. When not set, `nix run` would (typically
      # erroneously) deduce the executable name from the derivation name and
      # attempt to run, for example,
      # `/nix/store/...-project-exe-app-0.1.0.0/bin/project-exe-app` instead of
      # `/nix/store/...-project-exe-app-0.1.0.0/bin/app`.
      mainProgram = exeName;
    };

    propagatedBuildInputs =
         frameworks # Frameworks will be needed at link time
      # Not sure why pkgconfig needs to be propagatedBuildInputs but
      # for gi-gtk-hs it seems to help.
      ++ map pkgs.lib.getDev (builtins.concatLists pkgconfig)
      ++ lib.optionals (stdenv.hostPlatform.isWindows)
        (lib.flatten component.libs
        ++ map haskellLib.dependToLib component.depends);

    buildInputs = lib.optionals (!stdenv.hostPlatform.isWindows)
      (lib.flatten component.libs
      ++ map haskellLib.dependToLib component.depends);

    nativeBuildInputs =
      [shellWrappers buildPackages.removeReferencesTo]
      ++ executableToolDepends;

    outputs = ["out" ]
      ++ (lib.optional enableSeparateDataOutput "data")
      ++ (lib.optional keepSource "source")
      ++ (lib.optional writeHieFiles "hie");

    prePatch =
      # emcc is very slow if it cannot cache stuff in $HOME
      (lib.optionalString (stdenv.hostPlatform.isGhcjs) ''
      export HOME=$(mktemp -d)
      '') +
      (lib.optionalString (!canCleanSource) ''
      echo "Cleaning component source not supported, leaving it un-cleaned"
      '') +
      (lib.optionalString keepSource ''
        cp -r . $source
        cd $source
        chmod -R +w .
      '') + commonAttrs.prePatch;

    configurePhase = ''
      runHook preConfigure
      echo Configure flags:
      printf "%q " ${finalConfigureFlags}
      echo
      $SETUP_HS configure ${finalConfigureFlags}
      runHook postConfigure
    '';

    buildPhase =
      # It seems that by the time the iserv wrapper specifiec by `--ghc-option=-pgmi` runs
      # all the array environment variables are removed from the environment.  To get a list
      # of all the locations a DLLs might be present we need access to pkgsHostTarget.
      # Adding a string version of the list array of nix store paths allows us to get that
      # list when we need it.
      (lib.optionalString stdenv.hostPlatform.isWindows ''
        export pkgsHostTargetAsString="''${pkgsHostTarget[@]}"
      '') +
      # The following could be refactored but would lead to many rebuilds

      # In case of content addressed components we need avoid parallel building (passing -j1)
      # in order to have a deterministic output and therefore avoid potential situations
      # where the binary cache becomes useless
      # See also https://gitlab.haskell.org/ghc/ghc/-/issues/12935
      (if contentAddressed then ''
        runHook preBuild
        $SETUP_HS build ${haskellLib.componentTarget componentId} -j1 ${lib.concatStringsSep " " setupBuildFlags}
        runHook postBuild
      '' else if stdenv.hostPlatform.isGhcjs then ''
        runHook preBuild
        # https://gitlab.haskell.org/ghc/ghc/issues/9221
        $SETUP_HS build ${haskellLib.componentTarget componentId} ${lib.concatStringsSep " " setupBuildFlags}
        runHook postBuild
      '' else ''
        runHook preBuild
        # https://gitlab.haskell.org/ghc/ghc/issues/9221
        $SETUP_HS build ${haskellLib.componentTarget componentId} -j$(($NIX_BUILD_CORES > 4 ? 4 : $NIX_BUILD_CORES)) ${lib.concatStringsSep " " setupBuildFlags}
        runHook postBuild
      '');

    # Note: Cabal does *not* copy test executables during the `install` phase.
    #
    # Note 2: if a package contains multiple libs (lib + sublibs) SETUP register will generate a
    #         folder instead of a file for the configuration.  Therefore if the result is a folder,
    #         we need to register each and every element of that folder.
    #
    # Note 3: if a package has no libs SETUP will not generate anything.  This can
    #         happen when building the `all` component of a package.
    installPhase = let
        target-pkg-and-db = "${ghc.targetPrefix}ghc-pkg -v0 --package-db $out/package.conf.d";
      in ''
      runHook preInstall
      $SETUP_HS copy ${lib.concatStringsSep " " (
        setupInstallFlags
        ++ lib.optional configureAllComponents
              (haskellLib.componentTarget componentId)
      )}
      ${lib.optionalString (haskellLib.isLibrary componentId) ''
        $SETUP_HS register --gen-pkg-config=${name}.conf
        ${ghc.targetPrefix}ghc-pkg -v0 init $out/package.conf.d
        ${ghc.targetPrefix}ghc-pkg -v0 --package-db ${configFiles}/${configFiles.packageCfgDir} -f $out/package.conf.d register ${name}.conf

        mkdir -p $out/exactDep
        touch $out/exactDep/configure-flags
        touch $out/exactDep/cabal.config
        touch $out/envDep

        ${ # The main library in a package has the same name as the package
          if package.identifier.name == componentId.cname
            then ''
              if id=$(${target-pkg-and-db} field ${package.identifier.name} id --simple-output); then
                echo "--dependency=${package.identifier.name}=$id" >> $out/exactDep/configure-flags
                echo "package-id $id" >> $out/envDep
              else
                echo 'ERROR: ${package.identifier.name} id could not be found with ${target-pkg-and-db}'
                exit 1
              fi
              if ver=$(${target-pkg-and-db} field ${package.identifier.name} version --simple-output); then
                echo "constraint: ${package.identifier.name} == $ver" >> $out/exactDep/cabal.config
                echo "constraint: ${package.identifier.name} installed" >> $out/exactDep/cabal.config
              else
                echo 'ERROR: ${package.identifier.name} version could not be found with ${target-pkg-and-db}'
                exit 1
              fi
            ''
            else
              # If the component name is not the package name this must be a sublib.
              # As we build sublibs separately, the following query should be safe.
              (''
              if id=$(${target-pkg-and-db} field "z-${package.identifier.name}-z-*" id --simple-output); then
                name=$(${target-pkg-and-db} field "z-${package.identifier.name}-z-*" name --simple-output)
                echo "--dependency=''${name#z-${package.identifier.name}-z-}=$id" >> $out/exactDep/configure-flags
                ''
                # Allow `package-name:sublib-name` to work in `build-depends`
                # by adding the same `--dependency` again, but with the package
                # name added.
                + ''
                echo "--dependency=${package.identifier.name}:''${name#z-${package.identifier.name}-z-}=$id" >> $out/exactDep/configure-flags
              else
                echo 'ERROR: ${package.identifier.name} id could not be found with ${target-pkg-and-db}'
                exit 1
              fi
              '')
        }
      ''}
      ${(lib.optionalString (haskellLib.isTest componentId || haskellLib.isBenchmark componentId) ''
        mkdir -p $out/bin
        if [ -f ${testExecutable} ]; then
          mkdir -p $(dirname $out/bin/${exeName})
          ${if stdenv.hostPlatform.isGhcjs then ''
            cat <(echo \#!${lib.getBin buildPackages.nodejs-12_x}/bin/node) ${testExecutable} >| $out/bin/${exeName}
            chmod +x $out/bin/${exeName}
          '' else ''
             cp -r ${testExecutable} $(dirname $out/bin/${exeName})
          ''}
        fi
      '')
      # In case `setup copy` did not create this
      + (lib.optionalString enableSeparateDataOutput ''
         mkdir -p $data
      '')
      + (lib.optionalString (stdenv.hostPlatform.isWindows && (haskellLib.mayHaveExecutable componentId)) (''
        echo "Symlink libffi and gmp .dlls ..."
        for p in ${lib.concatStringsSep " " [ libffi gmp ]}; do
          find "$p" -iname '*.dll' -exec ln -s {} $out/bin \;
        done
        ''
        # symlink all .dlls into the local directory.
        + ''
        for p in $pkgsHostTargetAsString; do
          find "$p" -iname '*.dll' -exec ln -s {} $out/bin \;
          find "$p" -iname '*.dll.a' -exec ln -s {} $out/bin \;
        done
      ''))
      + (lib.optionalString doCoverage ''
        mkdir -p $out/share
        cp -r dist/hpc $out/share
        cp dist/setup-config $out/
      '')
      }
      runHook postInstall
    '' + (
      # Keep just the autogen files and package.conf.inplace package
      # DB (probably empty unless this is a library component).
      # We also have to remove any refernces to $out to avoid
      # circular references.
      lib.optionalString (configureAllComponents || keepSource) ''
        mv dist dist-tmp-dir
        mkdir -p dist/build
        if [ -d dist-tmp-dir/build/${componentId.cname}/autogen ]; then
          mv dist-tmp-dir/build/${componentId.cname}/autogen dist/build/
        elif [ -d dist-tmp-dir/build/autogen ]; then
          mv dist-tmp-dir/build/autogen dist/build/
        fi
        mv dist-tmp-dir/package.conf.inplace dist/
        if [ -d dist/build/autogen ]; then
          remove-references-to -t $out dist/build/autogen/*
        fi
        rm -rf dist-tmp-dir
      ''
    ) + (lib.optionalString (keepSource && haskellLib.isLibrary componentId) ''
        remove-references-to -t $out ${name}.conf
    '') + (lib.optionalString (haskellLib.isTest componentId) ''
      echo The test ${package.identifier.name}.components.tests.${componentId.cname} was built.  To run the test build ${package.identifier.name}.checks.${componentId.cname}.
    '');

    doInstallCheck = true;
    installCheckPhase = lib.optionalString (haskellLib.isLibrary componentId) ''
      if test -n "$(shopt -s nullglob; echo $out/package.conf.d/${name}-*.conf)"; then
          echo $out/package.conf.d/${name}-*.conf " is present"
      else
        echo "ERROR: $out/package.conf.d/${name}-*.conf was not created"
        exit 1
      fi
    '';

    shellHook = ''
      export PATH="${shellWrappers}/bin:$PATH"
      ${shellHookApplied}
    '';
  }
  // haskellLib.optionalHooks {
    # These are the hooks that are not needed by haddock (see commonAttrs for the ones that
    # are shared with the haddock derivation)
    inherit
      preBuild postBuild
      preInstall postInstall;
  }
  // lib.optionalAttrs (hardeningDisable != [] || stdenv.hostPlatform.isMusl) {
    hardeningDisable = hardeningDisable ++ lib.optional stdenv.hostPlatform.isMusl "pie";
  });
in drv; in self)
