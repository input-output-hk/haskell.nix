{ lib, stdenv, mkShell, glibcLocales, pkgconfig, ghcForComponent, makeConfigFiles, hsPkgs, hoogleLocal, haskellLib, buildPackages, compiler }:

{ # `packages` function selects packages that will be worked on in the shell itself.
  # These packages will not be built by `shellFor`, but their
  # dependencies will be included in the shell's `ghc-pkg list`.
  packages ? ps: builtins.attrValues (haskellLib.selectLocalPackages ps)
  # `components` function selects components that will be worked on in the shell itself.
  # By default `shellFor` will include the dependencies of all of the components
  # in the selected packages.  If only as subset of the components will be
  # worked on in the shell then we can pass a different `components` function
  # to select those.
, components ? ps: lib.concatMap haskellLib.getAllComponents (packages hsPkgs)
  # Additional packages to be added unconditionally
, additional ? _: []
, withHoogle ? true
, exactDeps ? false
, tools ? {}
, packageSetupDeps ? true
, enableDWARF ? false
, ... } @ args:

let
  # TODO find out why hoogle index creation can be made to work for cross compilers
  withHoogle' = withHoogle && !haskellLib.isCrossHost;

  selectedPackages = packages hsPkgs;
  additionalPackages = additional hsPkgs;
  selectedComponents = components hsPkgs;

  # The configs of all the selected components
  selectedConfigs = map (c: c.config) selectedComponents
    ++ lib.optionals packageSetupDeps (map (p: p.setup.config) selectedPackages);

  name = if lib.length selectedPackages == 1
    then "ghc-shell-for-${(lib.head selectedPackages).identifier.name}"
    else "ghc-shell-for-packages";

  # Given a `packages = [ a b ]` we construct a shell with the dependencies of `a` and `b`.
  #
  # But if `a` depends on `b`, we don't want to include `b`, because
  # - cabal will end up ignoring that built version;
  # - The user has indicated that's what they're working on, so they probably don't want to have to build
  #   it first (and it will change often).
  # Generally we never want to include any of (the components of) the selected packages as dependencies.
  #
  # Furthermore, if `a` depends on `b`, `a` will include the library component of `b` in its `buildInputs`
  # (to make `propagatedBuildInputs` of `pkgconfig-depends` work). So we also need to filter those
  # (the pkgconfig depends of `b` will still be included in the
  # system shell's `buildInputs` via `b`'s own `buildInputs`).
  # We have to do this slightly differently because we will be looking at the actual components rather
  # than the packages.

  # Given a list of packages, removes those which were selected as part of the shell.
  # We do this on the basis of their identifiers being the same, not direct equality (why?).
  removeSelectedPackages =
    # All the identifiers of the selected packages
    let selectedPackageIds = map (p: p.identifier) selectedPackages;
    in lib.filter (input: !(builtins.elem (input.identifier or null) selectedPackageIds));

  # Given a list of derivations, removes those which are components of packages which were selected as part of the shell.
  removeSelectedInputs =
    # All the components of the selected packages: we shouldn't add any of these as dependencies
    let selectedPackageComponents = map (x: x.name) (lib.concatMap haskellLib.getAllComponents selectedPackages);
    in lib.filter (input: !(builtins.elem input.name selectedPackageComponents));

  # We need to remove any dependencies which are selected packages (see above).
  # `depends` contains packages so we use 'removeSelectedPackages`.
  packageInputs = removeSelectedPackages (lib.concatMap (cfg: cfg.depends) selectedConfigs) ++ additionalPackages;

  # Add the system libraries and build tools of the selected haskell packages to the shell.
  # We need to remove any inputs which are selected components (see above).
  # `buildInputs`, `propagatedBuildInputs`, and `executableToolDepends`  contain component
  # derivations, not packages, so we use `removeSelectedInputs`).
  systemInputs = removeSelectedInputs (lib.concatMap
    (c: c.buildInputs ++ c.propagatedBuildInputs) selectedComponents);
  nativeBuildInputs = removeSelectedInputs
    (lib.concatMap (c: c.executableToolDepends) selectedComponents);

  # Set up a "dummy" component to use with ghcForComponent.
  component = {
    depends = packageInputs;
    libs = [];
    frameworks = [];
    doExactConfig = false;
  };
  configFiles = makeConfigFiles {
    fullName = args.name or name;
    identifier.name = name;
    inherit component;
  };
  ghcEnv = ghcForComponent {
    inherit configFiles;
    componentName = name;
    postInstall = lib.optionalString withHoogle' ''
      ln -s ${hoogleIndex}/bin/hoogle $out/bin
    '';
    inherit enableDWARF;
  };

  hoogleIndex = let
    # Get the doc package for a component, and add attributes that
    # hoogle.nix expects.
    docPackage = p: lib.getOutput "doc" p // {
      pname = p.identifier.name;
      haddockDir = lib.const p.haddockDir;
    };
  in hoogleLocal ({
    packages = map docPackage (haskellLib.flatLibDepends component);

    # Need to add hoogle to hsPkgs.
    # inherit (hsPkgs) hoogle;
  } // (
    lib.optionalAttrs (args ? tools && args.tools ? hoogle) {
      hoogle = buildPackages.haskell-nix.tool compiler.nix-name "hoogle" args.tools.hoogle;
    }
  ));

  mkDrvArgs = builtins.removeAttrs args ["packages" "additional" "withHoogle" "tools"];
in
  mkShell (mkDrvArgs // {
    name = mkDrvArgs.name or name;

    buildInputs = systemInputs
      ++ mkDrvArgs.buildInputs or [];
    nativeBuildInputs = [ ghcEnv ]
      ++ nativeBuildInputs
      ++ mkDrvArgs.nativeBuildInputs or []
      ++ lib.attrValues (buildPackages.haskell-nix.tools compiler.nix-name tools)
      # If this shell is a cross compilation shell include
      # wrapper script for running cabal build with appropriate args.
      ++ lib.optional (ghcEnv.targetPrefix != "") (
            buildPackages.writeShellScriptBin "${ghcEnv.targetPrefix}cabal" ''
              exec cabal \
                --with-ghc=${ghcEnv.targetPrefix}ghc \
                --with-ghc-pkg=${ghcEnv.targetPrefix}ghc-pkg \
                --with-hsc2hs=${ghcEnv.targetPrefix}hsc2hs \
                ${lib.optionalString (ghcEnv.targetPrefix == "js-unknown-ghcjs-") ''
                  --with-ghcjs=${ghcEnv.targetPrefix}ghc \
                  --with-ghcjs-pkg=${ghcEnv.targetPrefix}ghc-pkg \
                  --ghcjs \
                ''} $(builtin type -P "${ghcEnv.targetPrefix}pkg-config" &> /dev/null && echo "--with-pkg-config=${ghcEnv.targetPrefix}pkg-config") \
                "$@"
              '');
    phases = ["installPhase"];
    installPhase = ''
      echo "${"Shell for " + toString (builtins.map (p : p.identifier.name) selectedPackages)}"
      echo $nativeBuildInputs $buildInputs > $out
    '';
    LANG = "en_US.UTF-8";
    LOCALE_ARCHIVE = lib.optionalString (stdenv.hostPlatform.libc == "glibc") "${glibcLocales}/lib/locale/locale-archive";

    # This helps tools like `ghcide` (that use the ghc api) to find
    # the correct global package DB.
    NIX_GHC_LIBDIR = ghcEnv + "/" + configFiles.libDir;

    passthru = (mkDrvArgs.passthru or {}) // {
      ghc = ghcEnv;
      inherit configFiles;
    };
  } // lib.optionalAttrs exactDeps {
    CABAL_CONFIG = "${configFiles}/cabal.config";
  })
