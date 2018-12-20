# Test a package set
{ pkgs
, haskell
, stdenv
}:

with stdenv.lib;

let
  pkgSet = haskell.mkNewPkgSet {
    inherit pkgs;
    pkg-def = import ./plan.nix;
    pkg-def-overlays = [
      { cabal-simple = ./cabal-simple.nix;
      }
    ];
    modules = [ ];
  };

  packages = pkgSet.config.hsPkgs;

in
  stdenv.mkDerivation {
    name = "cabal-simple-test";

    buildCommand = ''
      exe="${packages.cabal-simple.components.exes.cabal-simple}/bin/cabal-simple"

      size=$(command stat --format '%s' "$exe")
      printf "size of executable $exe is $size. \n" >& 2

      # fixme: run on target platform when cross-compiled
      printf "checking whether executable runs... " >& 2
      $exe

      # fixme: linux-specific
      printf "checking that executable is dynamically linked to system libraries... " >& 2
      ldd $exe | grep libpthread

      touch $out
    '';

    meta.platforms = platforms.all;
} // { inherit (packages) cabal-simple; }

## steps to generate local files
# 1. cabal-to-nix cabal-simple.cabal > cabal-simple.nix
# 2. cabal new-build
# 3. plan-to-nix ./dist-newstyle/cache/plan.json > plan.nix
