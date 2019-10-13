{ stdenv, mkCabalProjectPkgSet, callCabalProjectToNix, importAndFilterProject }:

with stdenv.lib;

let
  plan = (importAndFilterProject (callCabalProjectToNix {
      index-state = "2019-04-30T00:00:00Z";
      src = ./.;
    }));
  pkgSet = mkCabalProjectPkgSet {
    plan-pkgs = plan.pkgs;
    # TODO find a way to get the ghc-options into plan.json so we can use it in plan-to-nix
    modules = [ { packages.test-ghc-options.package.ghcOptions = "-DTEST_GHC_OPTION"; } ];
  };
  packages = pkgSet.config.hsPkgs;
in
  stdenv.mkDerivation {
    name = "call-cabal-project-to-nix-test";

    buildCommand = ''
      exe="${packages.test-ghc-options.components.exes.test-ghc-options-exe}/bin/test-ghc-options-exe"

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
