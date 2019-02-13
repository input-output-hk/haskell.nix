{ stdenv, util, callCabalToNix, ghc }:

with stdenv.lib;

let
  pkgSet = callCabalToNix "cabal-simple" ../cabal-simple ghc;

  packages = pkgSet.config.hsPkgs;

in
  stdenv.mkDerivation {
    name = "ifd-test";

    buildCommand = ''
      exe="${packages.cabal-simple.components.exes.cabal-simple}/bin/cabal-simple"

      printf "checking whether executable runs... " >& 2
      $exe

      touch $out
    '';

    meta.platforms = platforms.all;

    passthru = {
      # Used for debugging with nix repl
      inherit pkgSet packages;
    };
  }
