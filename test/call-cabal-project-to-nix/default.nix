{ stdenv, mkCabalProjectPkgSet, callCabalProjectToNix, importAndFilterProject, recurseIntoAttrs }:

with stdenv.lib;

let
  # This test could use cabalProject', but it does so that it
  # tests using callCabalProjectToNix and importAndFilterProject
  plan = (importAndFilterProject (callCabalProjectToNix {
    name = "test-call-cabal-project-to-nix";
    index-state = "2019-04-30T00:00:00Z";
    # reuse the cabal-simple test project
    src = ../cabal-simple;
  }));
  pkgSet = mkCabalProjectPkgSet {
    plan-pkgs = plan.pkgs;
  };
  packages = pkgSet.config.hsPkgs;

in recurseIntoAttrs {
  plan-nix = plan.nix;
  run = stdenv.mkDerivation {
    name = "call-cabal-project-to-nix-test";

    buildCommand = ''
      exe="${packages.cabal-simple.components.exes.cabal-simple}/bin/cabal-simple${stdenv.hostPlatform.extensions.executable}"

      printf "checking whether executable runs... " >& 2
      cat ${packages.cabal-simple.components.exes.cabal-simple.run}

      touch $out
    '';

    meta.platforms = platforms.all;

    passthru = {
      # Attributes used for debugging with nix repl
      inherit pkgSet packages;
    };
  };
}