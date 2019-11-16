# Test a package set
{ stdenv, util, mkCabalProjectPkgSet, cabalProject', haskellLib }:

with stdenv.lib;

let
  modules = [
    {
      # Package has no exposed modules which causes
      #   haddock: No input file(s)
      packages.cabal-sublib.doHaddock = false;
    }
  ];

  # The ./pkgs.nix works for linux & darwin, but not for windows
  project = if stdenv.hostPlatform.isWindows
    then cabalProject' {
      name = "cabal-sublib";
      src = haskellLib.cleanGit { src = ../..; subDir = "test/cabal-sublib"; };
      inherit modules;
    }
    else rec {
      pkgSet = mkCabalProjectPkgSet {
        plan-pkgs = import ./pkgs.nix;
        inherit modules;
      };

      inherit (pkgSet.config) hsPkgs;
    };

  packages = project.hsPkgs;

in
  stdenv.mkDerivation {
    name = "cabal-sublib-test";

    buildCommand = ''
      exe="${packages.cabal-sublib.components.exes.cabal-sublib}/bin/cabal-sublib${stdenv.hostPlatform.extensions.executable}"

      size=$(command stat --format '%s' "$exe")
      printf "size of executable $exe is $size. \n" >& 2

      # fixme: run on target platform when cross-compiled
      printf "checking whether executable runs... " >& 2
      cat ${packages.cabal-sublib.components.exes.cabal-sublib.run}

      printf "checking that executable is dynamically linked to system libraries... " >& 2
    '' + optionalString stdenv.isLinux ''
      ldd $exe | grep libgmp
    '' + optionalString stdenv.isDarwin ''
      otool -L $exe |grep .dylib
    '' + ''

      printf "Checking that \"all\" component has the programs... " >& 2
      all_exe="${packages.cabal-sublib.components.all}/bin/cabal-sublib${stdenv.hostPlatform.extensions.executable}"
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
