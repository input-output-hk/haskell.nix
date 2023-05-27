{ stdenv, lib, buildPackages, mkCabalProjectPkgSet, callCabalProjectToNix, importAndFilterProject, recurseIntoAttrs, haskellLib, testSrc, compiler-nix-name, evalPackages }:

with lib;

let
  # This test could use cabalProject', but it does so that it
  # tests using callCabalProjectToNix and importAndFilterProject
  callProjectResults = callCabalProjectToNix {
    inherit compiler-nix-name evalPackages;
    # reuse the cabal-simple test project
    src = testSrc "cabal-simple";
    cabalProject = ''
      packages: .
      allow-newer: aeson:*
    '' + lib.optionalString (__elem compiler-nix-name ["ghc96020230302" "ghc961" "ghc962"]) ''
      allow-newer: *:base, *:ghc-prim, *:template-haskell
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
