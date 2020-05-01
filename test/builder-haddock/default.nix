{ mkCabalProjectPkgSet, stdenv, testSrc }:

with stdenv.lib;

let
  pkgSet = mkCabalProjectPkgSet {
    plan-pkgs = import ./pkgs.nix;
    modules = [
      # overrides to fix the build
      {
        packages.transformers-compat.components.library.doExactConfig = true;
      }

      {
        # Add a hook to the haddock phase
        packages.test-haddock.postHaddock = ''
          echo "==="
          echo "This is the postHaddock hook. The files are:"
          find .
          echo "==="
        '';

        # Check that the package option works
        packages.stm.doHaddock = false;
      }
    ];
  };

  packages = pkgSet.config.hsPkgs;

in
  stdenv.mkDerivation {
    name = "builder-haddock-test";

    buildCommand = let
      inherit (packages.test-haddock.components) library;
      noDocLibrary = packages.stm.components.library;
    in if (stdenv.hostPlatform != stdenv.buildPlatform)
      then ''
        echo "Skipping haddock tests when cross compiling" >& 2
        touch $out
      ''
      else ''
      ########################################################################
      # test haddock

      doc="${toString library.doc or null}"
      docDir="${toString library.haddockDir}"

      # exeDoc="$ disabled {toString packages.test-haddock.components.exes.test-haddock.doc}"
      # printf "checking that executable output does not have docs ... " >& 2
      # echo $exeDoc
      # test "$exeDoc" = ""

      printf "checking that documentation directory was built... " >& 2
      echo "$doc" >& 2
      test -n "$doc"

      printf "checking that documentation was generated... " >& 2
      grep hello "$docDir/TestHaddock.html" > /dev/null
      echo yes >& 2

      printf "checking for hoogle index of package... " >& 2
      grep hello "$docDir/test-haddock.txt" > /dev/null
      echo yes >& 2

      printf "checking for absence of documentation in another package... " >& 2
      if [ -d "${toString noDocLibrary.haddockDir}" ]; then
        echo "it exists - FAIL" >& 2
      else
        echo PASS >& 2
      fi

      printf "checking for absence of library package store paths in docs... " >& 2
      if grep -R ${library} "$docDir" > /dev/null; then
        echo "Found ${library} - FAIL" >& 2
        exit 1
      else
        echo "PASS" >& 2
      fi

      touch $out
    '';

    meta.platforms = platforms.all;
    meta.disabled = stdenv.hostPlatform.isMusl;
} // { inherit packages pkgSet; }
