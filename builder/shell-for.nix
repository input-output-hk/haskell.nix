{ buildPackages, lib, stdenv, ghcForComponent, makeConfigFiles, hsPkgs }:

{ packages, withHoogle ? true } @ args:

let
  selected = packages hsPkgs;
  name = if lib.length selected == 1
    then "ghc-shell-for-${(lib.head selected).name}"
    else "ghc-shell-for-packages";
  ghcEnv = ghcForComponent {
    componentName = name;
    configFiles = makeConfigFiles {
      fullName = args.name or name;
      identifier.name = name;
      component = {
        depends = selected;
        libs = [];
        frameworks = [];
        doExactConfig = false;
      };
    };
  };
  mkDrvArgs = builtins.removeAttrs args ["packages" "withHoogle"];

  # fixme: check if systemInputs and nativeBuildInputs are necessary
  systemInputs = [];
  nativeBuildInputs = [];
in
  stdenv.mkDerivation (mkDrvArgs // {
    name = mkDrvArgs.name or name;

    buildInputs = systemInputs ++ mkDrvArgs.buildInputs or [];
    nativeBuildInputs = [ ghcEnv ] ++ nativeBuildInputs ++ mkDrvArgs.nativeBuildInputs or [];
    phases = ["installPhase"];
    installPhase = "echo $nativeBuildInputs $buildInputs > $out";
    LANG = "en_US.UTF-8";
    LOCALE_ARCHIVE = lib.optionalString (stdenv.hostPlatform.libc == "glibc") "${buildPackages.glibcLocales}/lib/locale/locale-archive";

    passthru.ghc = ghcEnv;
  })
