{ stdenv, snapshots }:

with stdenv.lib;

let
  env = snapshots."lts-12.21".ghcWithHoogle
    (ps: with ps; [ conduit conduit-extra resourcet ]);

in
  stdenv.mkDerivation {
    name = "shell-for-test";

    buildCommand = ''
      ########################################################################
      # test snapshot ghcWithHoogle

      printf "checking that the ghcWithPackages env has the package...\n" >& 2
      ${env}/bin/ghc-pkg list | grep conduit

      touch $out
    '';

    meta.platforms = platforms.all;
    passthru = {
      inherit env;
    };
}
