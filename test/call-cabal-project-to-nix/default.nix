{ stdenv, lib, mkCabalProjectPkgSet, callCabalProjectToNix, importAndFilterProject, recurseIntoAttrs, haskellLib, testSrc, compiler-nix-name }:

with lib;

let
  # This test could use cabalProject', but it does so that it
  # tests using callCabalProjectToNix and importAndFilterProject
  callProjectResults = callCabalProjectToNix {
    inherit compiler-nix-name;
    # reuse the cabal-simple test project
    src = testSrc "cabal-simple";
  };
  pkgSet = mkCabalProjectPkgSet {
    plan-pkgs = importAndFilterProject {
      inherit (callProjectResults) projectNix sourceRepos src;
    };
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

    meta.platforms = platforms.all;

    passthru = {
      # Attributes used for debugging with nix repl
      inherit pkgSet packages;
    };
  };
}
