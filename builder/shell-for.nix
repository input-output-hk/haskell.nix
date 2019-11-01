{ lib, stdenv, glibcLocales, pkgconfig, ghcForComponent, makeConfigFiles, hsPkgs, hoogleLocal, haskellLib }:

{ packages ? ps:
    let
      selected = haskellLib.selectLocalPackages ps;
    in
      builtins.trace ("Shell for " + toString (builtins.attrNames selected))
        (builtins.attrValues selected)
, additional ? _: []
, withHoogle ? true
, exactDeps ? false
, ... } @ args:

let
  selected = packages hsPkgs;
  additionalSelected = additional hsPkgs;
  selectedConfigs = map (p: p.components.all.config) selected;

  name = if lib.length selected == 1
    then "ghc-shell-for-${(lib.head selected).identifier.name}"
    else "ghc-shell-for-packages";

  # Removes the selected packages from a list of packages.
  # If `packages = [ a b ]` and `a` depends on `b`, don't build `b`,
  # because cabal will end up ignoring that built version;
  removeSelected = lib.filter
    (input: lib.all (cfg: (input.identifier or null) != cfg.identifier) selected);

  packageInputs = removeSelected
    (lib.concatMap (cfg: cfg.depends) selectedConfigs
    ++ additionalSelected
    ++ lib.concatMap (cfg: cfg.setup.config.depends or []) selected
    );

  # Add the system libraries and build tools of the selected haskell
  # packages to the shell.
  systemInputs = lib.concatMap (p: p.components.all.buildInputs) selected;
  nativeBuildInputs = removeSelected
    (lib.concatMap (p: p.components.all.executableToolDepends) selected);

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
    postInstall = lib.optionalString withHoogle ''
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

  mkDrvArgs = builtins.removeAttrs args ["packages" "additional" "withHoogle"];
in
  stdenv.mkDerivation (mkDrvArgs // {
    name = mkDrvArgs.name or name;

    buildInputs = systemInputs
      ++ mkDrvArgs.buildInputs or []
      ++ lib.optional withHoogle hoogleIndex;
    nativeBuildInputs = [ ghcEnv ]
      ++ nativeBuildInputs
      ++ mkDrvArgs.nativeBuildInputs or [];
    phases = ["installPhase"];
    installPhase = "echo $nativeBuildInputs $buildInputs > $out";
    LANG = "en_US.UTF-8";
    LOCALE_ARCHIVE = lib.optionalString (stdenv.hostPlatform.libc == "glibc") "${glibcLocales}/lib/locale/locale-archive";

    passthru = (mkDrvArgs.passthru or {}) // {
      ghc = ghcEnv;
    };
  } // lib.optionalAttrs exactDeps {
    CABAL_CONFIG = "${configFiles}/cabal.config";
  })
