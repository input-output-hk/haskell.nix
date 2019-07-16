{ stdenv, mkCabalProjectPkgSet, callCabalProjectToNix }:

with stdenv.lib;

let
  pkgSet = mkCabalProjectPkgSet {
    plan-pkgs = import (callCabalProjectToNix {
      index-state = "2019-04-30T00:00:00Z";
      # reuse the cabal-simple test project
      src = ../cabal-simple;
    });
  };
  packages = pkgSet.config.hsPkgs;
in
  stdenv.mkDerivation {
    name = "call-cabal-project-to-nix-test";

    buildCommand = ''
      exe="${packages.cabal-simple.components.exes.cabal-simple}/bin/cabal-simple"

      printf "checking whether executable runs... " >& 2
      $exe

      touch $out
    '';

    meta.platforms = platforms.all;

    passthru = {
      # Attributes used for debugging with nix repl
      inherit pkgSet packages;
    };
  }
