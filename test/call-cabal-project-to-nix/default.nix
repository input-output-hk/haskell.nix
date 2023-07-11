{ stdenv, lib, buildPackages, mkCabalProjectPkgSet, callCabalProjectToNix, importAndFilterProject, recurseIntoAttrs, haskellLib, testSrc, compiler-nix-name, evalPackages }:

with lib;

let
  # This test could use cabalProject', but it does so that it
  # tests using callCabalProjectToNix and importAndFilterProject
  callProjectResults = callCabalProjectToNix {
    inherit compiler-nix-name evalPackages;
    # reuse the cabal-simple test project
    src = testSrc "cabal-simple";
    cabalProjectLocal = lib.optionalString (__elem compiler-nix-name ["ghc9820230704"]) ''
      source-repository-package
        type: git
        location: https://github.com/glguy/th-abstraction.git
        tag: 24b9ea9b498b182e44abeb3a755e2b4e35c48788
        --sha256: sha256-nWWZVEek0fNVRI+P5oXkuJyrPJWts5tCphymFoYWIPg=
    '';
  };
  pkgSet = mkCabalProjectPkgSet {
    plan-pkgs = importAndFilterProject {
      inherit (callProjectResults) projectNix sourceRepos src;
    };
    modules = [{ inherit evalPackages; }];
  };
  packages = pkgSet.config.hsPkgs;

in recurseIntoAttrs {
  ifdInputs = {
    plan-nix = callProjectResults.projectNix;
  };
  run = stdenv.mkDerivation {
    name = "call-cabal-project-to-nix-test";

    buildCommand = ''
      exe="${packages.cabal-simple.components.exes.cabal-simple.exePath}"

      printf "checking whether executable runs... " >& 2
      cat ${haskellLib.check packages.cabal-simple.components.exes.cabal-simple}/test-stdout

      touch $out
    '';

    meta = rec {
      platforms = lib.platforms.all;
      broken = stdenv.hostPlatform.isGhcjs && __elem compiler-nix-name ["ghc961" "ghc962"];
      disabled = broken;
    };

    passthru = {
      # Attributes used for debugging with nix repl
      inherit pkgSet packages;
    };
  };
}
