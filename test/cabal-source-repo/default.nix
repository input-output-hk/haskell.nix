{ stdenv, mkCabalProjectPkgSet, callCabalProjectToNix, importAndFilterProject }:

with stdenv.lib;

let
  pkgSet = mkCabalProjectPkgSet {
    plan-pkgs = (importAndFilterProject (callCabalProjectToNix {
      name = "test-cabal-source-repo";
      index-state = "2019-04-30T00:00:00Z";
      # reuse the cabal-simple test project
      src = ./.;
    })).pkgs;
  };
  packages = pkgSet.config.hsPkgs;
in
  stdenv.mkDerivation {
    name = "call-cabal-project-to-nix-test";

    buildCommand = ''
      exe="${packages.use-cabal-simple.components.exes.use-cabal-simple}/bin/use-cabal-simple${stdenv.hostPlatform.extensions.executable}"

      printf "checking whether executable runs... " >& 2
      cat ${packages.use-cabal-simple.components.exes.use-cabal-simple.run}

      touch $out
    '';

    meta.platforms = platforms.all;

    passthru = {
      # Attributes used for debugging with nix repl
      inherit pkgSet packages;
    };
  }
