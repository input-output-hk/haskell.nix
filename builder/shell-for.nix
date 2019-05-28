{ lib, stdenv, glibcLocales, pkgconfig, ghcForComponent, makeConfigFiles, hsPkgs }:

{ packages, withHoogle ? true, ... } @ args:

let
  selected = packages hsPkgs;
  selectedConfigs = map (p: p.components.all.config) selected;

  name = if lib.length selected == 1
    then "ghc-shell-for-${(lib.head selected).name}"
    else "ghc-shell-for-packages";

  # If `packages = [ a b ]` and `a` depends on `b`, don't build `b`,
  # because cabal will end up ignoring that built version, assuming
  # new-style commands.
  packageInputs = lib.filter
    (input: lib.all (cfg: input.identifier != cfg.identifier) selected)
    (lib.concatMap (cfg: cfg.depends) selectedConfigs);

  # Add the system libraries and build tools of the selected haskell
  # packages to the shell.
  systemInputs = lib.concatMap (p: p.components.all.buildInputs) selected;
  nativeBuildInputs = lib.concatMap (p: p.components.all.executableToolDepends) selected;

  # Set up a "dummy" component to use with ghcForComponent.
  configFiles = makeConfigFiles {
    fullName = args.name or name;
    identifier.name = name;
    component = {
      depends = packageInputs;
      libs = [];
      frameworks = [];
      doExactConfig = false;
    };
  };
  ghcEnv = ghcForComponent {
    componentName = name;
    inherit configFiles;
  };
  mkDrvArgs = builtins.removeAttrs args ["packages" "withHoogle"];
in
  stdenv.mkDerivation (mkDrvArgs // {
    name = mkDrvArgs.name or name;

    buildInputs = systemInputs ++ mkDrvArgs.buildInputs or [];
    nativeBuildInputs = [ ghcEnv ] ++ nativeBuildInputs ++ mkDrvArgs.nativeBuildInputs or [];
    phases = ["installPhase"];
    installPhase = "echo $nativeBuildInputs $buildInputs > $out";
    LANG = "en_US.UTF-8";
    LOCALE_ARCHIVE = lib.optionalString (stdenv.hostPlatform.libc == "glibc") "${glibcLocales}/lib/locale/locale-archive";
    CABAL_CONFIG = "${configFiles}/cabal.config";

    passthru.ghc = ghcEnv;
  })
