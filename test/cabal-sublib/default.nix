# Test a package set
{ stdenv, util, mkPkgSet }:

with stdenv.lib;

let
  ## steps to generate local files
  # 1. cabal-to-nix cabal-sublib.cabal > cabal-sublib.nix
  # 2. cabal new-build
  # 3. plan-to-nix ./dist-newstyle/cache/plan.json > plan.nix
  pkgSet = mkPkgSet {
    pkg-def = import ./plan.nix;
    pkg-def-overlays = [
      { cabal-sublib = ./cabal-sublib.nix;
      }
    ];
    modules = [
     {
       # Package has no exposed modules which causes
       #   haddock: No input file(s)
       packages.cabal-sublib.doHaddock = false;
     }
    ];
  };

  packages = pkgSet.config.hsPkgs;

in
  stdenv.mkDerivation {
    name = "cabal-sublib-test";

    buildCommand = ''
      exe="${packages.cabal-sublib.components.exes.cabal-sublib}/bin/cabal-sublib"

      size=$(command stat --format '%s' "$exe")
      printf "size of executable $exe is $size. \n" >& 2

      # fixme: run on target platform when cross-compiled
      printf "checking whether executable runs... " >& 2
      $exe

      printf "checking that executable is dynamically linked to system libraries... " >& 2
    '' + optionalString stdenv.isLinux ''
      ldd $exe | grep libpthread
    '' + optionalString stdenv.isDarwin ''
      otool -L $exe |grep .dylib
    '' + ''

      printf "Checking that \"all\" component has the programs... " >& 2
      all_exe="${packages.cabal-sublib.components.all}/bin/cabal-sublib"
      test -f "$all_exe"
      echo "$all_exe" >& 2

      touch $out
    '';

    meta.platforms = platforms.all;

    passthru = {
      # Used for debugging with nix repl
      inherit pkgSet packages;

      # Used for testing externally with nix-shell (../tests.sh).
      # This just adds cabal-install to the existing shells.
      test-shell = util.addCabalInstall packages.cabal-sublib.components.all;
    };
}
