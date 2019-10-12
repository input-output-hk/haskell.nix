{ stdenv, mkCabalProjectPkgSet, callCabalProjectToNix, importAndFilterProject }:

with stdenv.lib;

let
  plan = (importAndFilterProject (callCabalProjectToNix {
      index-state = "2019-04-30T00:00:00Z";
      src = ./.;
    }));
  pkgSet = mkCabalProjectPkgSet {
    plan-pkgs = plan.pkgs;
  };
  packages = pkgSet.config.hsPkgs;
in
  stdenv.mkDerivation {
    name = "call-cabal-project-to-nix-test";

    buildCommand = ''
      exe="${packages.test-project-flags.components.exes.test-project-flags-exe}/bin/test-project-flags-exe"

      printf "checking whether executable runs... " >& 2
      $exe

      touch $out
    '';

    meta.platforms = platforms.all;

    passthru = {
      # Attributes used for debugging with nix repl
      inherit pkgSet packages;
      plan-nix = plan.nix;
    };
  }
