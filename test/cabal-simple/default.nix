# Test a package set
{ stdenv, util, mkCabalProjectPkgSet, cabalProject', haskellLib, recurseIntoAttrs }:

with stdenv.lib;

let
  modules = [
     {
       # Package has no exposed modules which causes
       #   haddock: No input file(s)
       packages.cabal-simple.doHaddock = false;
     }
  ];

  project = cabalProject' {
    src = haskellLib.cleanGit { src = ../..; subDir = "test/cabal-simple"; };
    inherit modules;
  };

  packages = project.hsPkgs;

in recurseIntoAttrs {
  ifdInputs = {
    inherit (project) plan-nix;
  };

  # Used for testing externally with nix-shell (../tests.sh).
  # This just adds cabal-install to the existing shells.
  test-shell = util.addCabalInstall packages.cabal-simple.components.all;

  run = stdenv.mkDerivation {
    name = "cabal-simple-test";

    buildCommand = ''
      exe="${packages.cabal-simple.components.exes.cabal-simple}/bin/cabal-simple${stdenv.hostPlatform.extensions.executable}"

      size=$(command stat --format '%s' "$exe")
      printf "size of executable $exe is $size. \n" >& 2

      # fixme: run on target platform when cross-compiled
      printf "checking whether executable runs... " >& 2
      cat ${haskellLib.check packages.cabal-simple.components.exes.cabal-simple}
    '' + (if stdenv.hostPlatform.isMusl
      then ''
        printf "checking that executable is statically linked... " >& 2
        (ldd $exe 2>&1 || true) | grep -i "not a"
      ''
      else
        # Skip this on aarch as we do not have an `ldd` tool
        optionalString (!stdenv.hostPlatform.isAarch32 && !stdenv.hostPlatform.isAarch64) (''
          printf "checking that executable is dynamically linked to system libraries... " >& 2
        '' + optionalString stdenv.isLinux ''
          ldd $exe | grep libpthread
        '' + optionalString stdenv.isDarwin ''
          otool -L $exe |grep .dylib
      '')) + ''

      printf "Checking that \"all\" component has the programs... " >& 2
      all_exe="${packages.cabal-simple.components.all}/bin/cabal-simple${stdenv.hostPlatform.extensions.executable}"
      test -f "$all_exe"
      echo "$all_exe" >& 2

      touch $out
    '';

    meta.platforms = platforms.all;

    passthru = {
      # Used for debugging with nix repl
      inherit project packages;
    };
  };
}
