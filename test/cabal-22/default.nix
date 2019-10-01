{ stdenv, mkCabalProjectPkgSet, util }:

with stdenv.lib;

let
  pkgSet = mkCabalProjectPkgSet {
    plan-pkgs = import ./pkgs.nix;
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

      printf "checking that executable is dynamically linked to system libraries... " >& 2
    '' + optionalString stdenv.isLinux ''
      ldd $exe | grep libgmp
    '' + optionalString stdenv.isDarwin ''
      otool -L $exe | grep "libSystem.B"
    '' + ''
      # fixme: posix-specific
      printf "checking that dynamic library is produced... " >& 2
    '' + optionalString stdenv.isLinux ''
      sofile=$(find "${packages.project.components.library}" | grep -e '\.so$')
    '' + optionalString stdenv.isDarwin ''
      sofile=$(find "${packages.project.components.library}" | grep -e '\.dylib$')
    '' + ''
      echo "$sofile"
    '' + optionalString (!stdenv.hostPlatform.isMusl) (''
      printf "checking that dynamic library is dynamically linked to prim... " >& 2
    '' + optionalString stdenv.isLinux ''
      ldd $sofile | grep libHSghc-prim
    '' + optionalString stdenv.isDarwin ''
      otool -L $sofile | grep libHSghc-prim
    '') + ''
      touch $out

      printf "checking whether benchmark runs... " >& 2
      ${packages.project.components.benchmarks.project-bench}/*/project-bench

      printf "checking whether tests run... " >& 2
      ${packages.project.components.tests.unit}/*/unit
    '';

    meta.platforms = platforms.all;
    passthru = {
      inherit (packages) project;
      shell = util.addCabalInstall packages.project.components.all;
    };
  }
