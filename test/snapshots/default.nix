{ stdenv, lib, haskellPackages, snapshots, runCommand, testSrc, compiler-nix-name, evalPackages }:

with lib;

let
  env = snapshots."lts-14.13".ghcWithHoogle
    (ps: with ps; [ conduit conduit-extra resourcet ]);

in lib.recurseIntoAttrs {
  # Does not work on ghcjs because it needs zlib.
  meta.disabled = stdenv.hostPlatform.isGhcjs || compiler-nix-name != "ghc865";
  inherit env;
  run = stdenv.mkDerivation {
    name = "shell-for-test";

    buildCommand = ''
      ########################################################################
      # test single component from haskellPackages

      printf "checking that the latest LTS snapshot has the lens package...\n" >& 2
      test -d ${haskellPackages.lens.components.library}

      ########################################################################
      # test snapshot ghcWithHoogle

      printf "checking that the ghcWithPackages env has the package...\n" >& 2
      ${env}/bin/${env.targetPrefix}ghc-pkg list | grep conduit

    ''
    # Hoogle support is currently disabled in cross compiler shells
    + (optionalString (stdenv.buildPlatform == stdenv.hostPlatform) ''
        printf "checking that the ghcWithPackages env has a hoogle index...\n" >& 2
        ${env}/bin/hoogle search Conduit --count=100 | grep ConduitT
    '') +''

      touch $out
    '';

    meta.platforms = platforms.all;
    passthru = {
      inherit env;
    };
  };
}
