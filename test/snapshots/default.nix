{ stdenv, haskellPackages }:

with stdenv.lib;

  stdenv.mkDerivation {
    name = "shell-for-test";

    buildCommand = ''
      ########################################################################
      # test snapshot ghcWithHoogle

      printf "checking that the latest LTS snapshot has the lens package...\n" >& 2
      test -d ${haskellPackages.lens.components.library}

      touch $out
    '';

    meta.platforms = platforms.all;
  }
