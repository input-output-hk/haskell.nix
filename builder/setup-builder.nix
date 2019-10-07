{ stdenv, lib, buildPackages, haskellLib, ghc, nonReinstallablePkgs, hsPkgs, makeSetupConfigFiles }:

{ setup-depends, package, name, src, flags, defaultSetupSrc
, preUnpack ? null, postUnpack ? null
}:

let
  component = {
    depends = setup-depends;
    libs = [];
    frameworks = [];
    doExactConfig = false;
    # We have to set hsSourceDirs or cleanCabalComponent will
    # include everything (and as a result all the components of
    # the package will depend on eveything in the package).
    # TODO find a better way
    hsSourceDirs = ["setup-src"];
    includeDirs = [];
    asmSources = [];
    cSources = [];
    cmmSources = [];
    cxxSources = [];
    jsSources = [];
    extraSrcFiles = [ "Setup.hs" "Setup.lhs" ];
  };
  cleanSrc = haskellLib.cleanCabalComponent package component src;

  fullName = "${name}-setup";

  includeGhcPackage = lib.any (p: p.identifier.name == "ghc") setup-depends;

  configFiles = makeSetupConfigFiles {
    inherit (package) identifier;
    inherit fullName flags component;
  };
  hooks = haskellLib.optionalHooks {
    inherit preUnpack postUnpack;
  };

in
 stdenv.lib.fix (drv:
    stdenv.mkDerivation ({
      name = "${fullName}";
      src = cleanSrc;
      nativeBuildInputs = [ghc];

      passthru = {
        inherit (package) identifier;
        config = component;
        inherit configFiles cleanSrc;
      };

      CABAL_CONFIG = configFiles + /cabal.config;
      phases = ["unpackPhase" "buildPhase" "installPhase"];
      buildPhase = ''
        if [[ ! -f ./Setup.hs  && ! -f ./Setup.lhs ]]; then
          cat ${defaultSetupSrc} > Setup.hs
        fi
        for f in Setup.hs Setup.lhs; do
          if [ -f $f ]; then
            echo Compiling package $f
            ghc $f -threaded '' + (if includeGhcPackage then "-package ghc " else "")
                + ''-package-db ${configFiles}/package.conf.d --make -o ./Setup
            setup=$(pwd)/Setup
          fi
        done
        [ -f ./Setup ] || (echo Failed to build Setup && exit 1)
      '';

      installPhase = ''
        mkdir -p $out/bin
        install ./Setup $out/bin/Setup
      '';
    } // hooks)
  )
