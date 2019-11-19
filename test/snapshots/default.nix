{ stdenv, haskellPackages, snapshots, recurseIntoAttrs, runCommand }:

with stdenv.lib;

let
  env = snapshots."lts-14.13".ghcWithHoogle
    (ps: with ps; [ conduit conduit-extra resourcet ]);

in recurseIntoAttrs (if stdenv.hostPlatform.isWindows
 then
    let skip = runCommand "skip-test-snapshot" {} ''
      echo "Skipping snapshot test on windows as does not work yet" >& 2
      touch $out
    '';
    in {
      env = skip;
      run = skip;
    }
 else {
  inherit env;
  run = stdenv.mkDerivation {
    name = "shell-for-test";

    buildCommand = if stdenv.hostPlatform.isWindows
      then ''
        printf "snapshot test is not working yet for windows. skipping. " >& 2
        touch $out
      ''
      else ''
      ########################################################################
      # test single component from haskellPackages

      printf "checking that the latest LTS snapshot has the lens package...\n" >& 2
      test -d ${haskellPackages.lens.components.library}

      ########################################################################
      # test snapshot ghcWithHoogle

      printf "checking that the ghcWithPackages env has the package...\n" >& 2
      ${env}/bin/ghc-pkg list | grep conduit

      printf "checking that the ghcWithPackages env has a hoogle index...\n" >& 2
      ${env}/bin/hoogle search Conduit --count=100 | grep ConduitT

      touch $out
    '';

    meta.platforms = platforms.all;
    passthru = {
      inherit env;
    };
  };
})
