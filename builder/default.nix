{ pkgs, stdenv, lib, haskellLib, ghc, fetchurl, writeText, runCommand, pkgconfig }:

{ flags
, package
, components

, name
, sha256
, src
, revision
, revisionSha256

, ...
}@config:

let
  cabalFile = if revision == null || revision == 0 then null else
    fetchurl {
      url = "https://hackage.haskell.org/package/${name}/revision/${toString revision}.cabal";
      sha256 = revisionSha256;
    };
  defaultSetupSrc = builtins.toFile "Setup.hs" ''
    import Distribution.Simple
    main = defaultMain
  '';
  defaultSetup = runCommand "default-Setup" { nativeBuildInputs = [ghc]; } ''
    cat ${defaultSetupSrc} > Setup.hs
    mkdir -p $out/bin
    ghc Setup.hs --make -o $out/bin/Setup
  '';

  setup = if package.buildType == "Simple"
    then defaultSetup
    else stdenv.mkDerivation {
      name = "${name}-setup";
      nativeBuildInputs = [ghc];
      inherit src;
      phases = ["unpackPhase" "buildPhase" "installPhase"];
      buildPhase = ''
        for f in Setup.hs Setup.lhs; do
          if [ -f $f ]; then
            echo Compiling package $f
            ghc $f --make -o ./Setup
            setup=$(pwd)/Setup
          fi
        done
        [ -f ./Setup ] || (echo Failed to build Setup && exit 1)
      '';

      installPhase = ''
        mkdir -p $out/bin
        install ./Setup $out/bin/Setup
      '';
    };

  comp-builder = haskellLib.weakCallPackage pkgs ./comp-builder.nix { inherit ghc haskellLib; };

  buildComp = componentId: component: comp-builder {
    inherit componentId component package name src flags setup cabalFile;
  };

in {
  components = haskellLib.applyComponents buildComp config;
  inherit (package) identifier;
  inherit setup cabalFile;
  isHaskell = true;
}
