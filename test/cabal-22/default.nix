{ pkgs
, haskell
, stdenv
}:

with stdenv.lib;

let
  pkgSet = haskell.mkPkgSet {
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
    '' + pkgs.lib.optionalString pkgs.stdenv.isLinux ''
      ldd $exe | grep libpthread
    '' + pkgs.lib.optionalString pkgs.stdenv.isDarwin ''
      otool -L $exe | grep "libSystem.B"
    '' + ''
      # fixme: posix-specific
      printf "checking that dynamic library is produced... " >& 2
    '' + pkgs.lib.optionalString pkgs.stdenv.isLinux ''
      sofile=$(find "${packages.project.components.library}" | grep -e '\.so$')
    '' + pkgs.lib.optionalString pkgs.stdenv.isDarwin ''
      sofile=$(find "${packages.project.components.library}" | grep -e '\.dylib$')
    '' + ''
      echo "$sofile"
      printf "checking that dynamic library is dynamically linked to prim... " >& 2
    '' + pkgs.lib.optionalString pkgs.stdenv.isLinux ''
      ldd $sofile | grep libHSghc-prim
    '' + pkgs.lib.optionalString pkgs.stdenv.isDarwin ''
      otool -L $sofile | grep libHSghc-prim
    '' + ''
      touch $out
    '';

    meta.platforms = platforms.all;
} // { inherit (packages) project; }
