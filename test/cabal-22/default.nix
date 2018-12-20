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
      { project = ./project.nix; }
    ];
    modules = [ ];
  };

  packages = pkgSet.config.hsPkgs;

in
  stdenv.mkDerivation {
    name = "cabal-22-test";

    buildCommand = ''
      exe="${packages.project.components.exes.project}/bin/project"

      size=$(command stat --format '%s' "$exe")
      printf "size of executable $exe is $size. \n" >& 2

      # fixme: run on target platform when cross-compiled
      printf "checking whether executable runs... " >& 2
      "$exe"

      # fixme: linux-specific
      printf "checking that executable is dynamically linked to system libraries... " >& 2
      ldd "$exe" | grep libpthread

      # fixme: posix-specific
      printf "checking that dynamic library is produced... " >& 2
      sofile=$(find "${packages.project.components.library}" | grep -e '\.so$')
      echo "$sofile"

      printf "checking that dynamic library is dynamically linked to base... " >& 2
      ldd "$sofile" | grep libHSbase

      touch $out
    '';

    meta.platforms = platforms.all;
} // { inherit (packages) project; }
