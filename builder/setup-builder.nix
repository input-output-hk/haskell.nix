{ stdenv, lib, buildPackages, haskellLib, ghc, nonReinstallablePkgs, hsPkgs, makeSetupConfigFiles }:

{ setup-depends, package, name, src, flags, postUnpack }:

let
  fullName = "${name}-setup";

  includeGhcPackage = lib.any (p: p.identifier.name == "ghc") setup-depends;

  configFiles = makeSetupConfigFiles {
    inherit (package) identifier;
    inherit fullName flags;
    component = {
      depends = setup-depends;
      libs = [];
      frameworks = [];
      doExactConfig = false;
    };
  };

in
 stdenv.lib.fix (drv:
    stdenv.mkDerivation {
      name = "${fullName}";
      inherit src;
      nativeBuildInputs = [ghc];

      CABAL_CONFIG = configFiles + /cabal.config;
      phases = ["unpackPhase" "buildPhase" "installPhase"];
      buildPhase = ''
        for f in Setup.hs Setup.lhs; do
          if [ -f $f ]; then
            echo Compiling package $f
            ghc $f '' + (if includeGhcPackage then "-package ghc " else "")
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
         inherit postUnpack;
       })
