{ lib, stdenv, glibcLocales, pkgconfig, ghcForComponent, makeConfigFiles, hsPkgs, hoogleLocal, haskellLib, buildPackages, compiler }:

{ # `packages` function selects packages that will be worked on in the shell itself.
  # These packages will not be built by `shellFor`, but their
  # dependencies will be included in the shell's `ghc-pkg list`.
  packages ? ps:
    let
      selected = haskellLib.selectLocalPackages ps;
    in
      builtins.trace ("Shell for " + toString (builtins.attrNames selected))
        (builtins.attrValues selected)
  # `components` function selects components that will be worked on in the shell itself.
  # By default `shellFor` will include the dependencies of all of the components
  # in the selected packages.  If only as subset of the components will be
  # worked on in the shell then we can pass a different `components` function
  # to select those.
, components ? ps: lib.concatMap haskellLib.getAllComponents (packages hsPkgs) 
, additional ? _: []
, withHoogle ? true
, exactDeps ? false
, tools ? {}
, ... } @ args:

let
  # TODO find out why hoogle index creation can be made to work for cross compilers
  withHoogle' = withHoogle && !haskellLib.isCrossHost;
  selected = packages hsPkgs;
  selectedComponents = components hsPkgs;
  additionalSelected = additional hsPkgs;
  selectedConfigs = map (c: c.config) selectedComponents;

  name = if lib.length selected == 1
    then "ghc-shell-for-${(lib.head selected).identifier.name}"
    else "ghc-shell-for-packages";

  # Removes the selected packages from a list of packages.
  # If `packages = [ a b ]` and `a` depends on `b`, don't build `b`,
  # because cabal will end up ignoring that built version;
  removeSelected = lib.filter
    (input: lib.all (cfg: (input.identifier or null) != cfg.identifier) selected);
  # Also since a will include the library component of b in its buildInputs
  # (to make `propagatedBuildInputs` of `pkgconfig-depends` work) those should
  # also be excluded (the pkgconfig depends of b will still be included in the
  # system shells buildInputs via b's own buildInputs).
  removeSelectedInputs = lib.filter
    (input: lib.all (cfg: input != cfg.components.library or null) selected);

  packageInputs =
    removeSelected
      (lib.concatMap (cfg: cfg.depends) selectedConfigs
      ++ lib.concatMap (cfg: cfg.setup.config.depends or []) selected
      )
    ++ additionalSelected;

  # Add the system libraries and build tools of the selected haskell
  # packages to the shell.
  systemInputs = removeSelectedInputs (lib.concatMap
    (c: c.buildInputs ++ c.propagatedBuildInputs) selectedComponents);
  nativeBuildInputs = removeSelected
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
  };

  hoogleIndex = let
    # Get the doc package for a component, and add attributes that
    # hoogle.nix expects.
    docPackage = p: lib.getOutput "doc" p // {
      pname = p.identifier.name;
      haddockDir = lib.const p.haddockDir;
    };
  in hoogleLocal {
    packages = map docPackage (haskellLib.flatLibDepends component);

    # Need to add hoogle to hsPkgs.
    # inherit (hsPkgs) hoogle;
  };

  mkDrvArgs = builtins.removeAttrs args ["packages" "additional" "withHoogle" "tools"];
in
  stdenv.mkDerivation (mkDrvArgs // {
    name = mkDrvArgs.name or name;

    buildInputs = systemInputs
      ++ mkDrvArgs.buildInputs or []
      ++ lib.optional withHoogle' hoogleIndex;
    nativeBuildInputs = [ ghcEnv ]
      ++ nativeBuildInputs
      ++ mkDrvArgs.nativeBuildInputs or []
      ++ lib.attrValues (buildPackages.haskell-nix.tools compiler.nix-name tools);
    phases = ["installPhase"];
    installPhase = "echo $nativeBuildInputs $buildInputs > $out";
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
