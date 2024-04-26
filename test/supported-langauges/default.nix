{ stdenv, pkgs, lib, recurseIntoAttrs, testSrc, compiler-nix-name, evalPackages, buildPackages }:

let
  ghc = buildPackages.haskell-nix.compiler.${compiler-nix-name};

  supported-langauges = import ../../lib/supported-languages.nix {
    inherit pkgs evalPackages ghc;
  };

in recurseIntoAttrs {
  check = stdenv.mkDerivation {
    name = "support-languages-check";

    buildCommand = ''
      expected=$(mktemp)
      ${ghc}/bin/${ghc.targetPrefix}ghc --supported-languages >$expected

      echo 'diff -u $expected ${supported-langauges}'
      diff -u $expected ${supported-langauges}

      touch $out
    '';

    meta = rec {
      platforms = lib.platforms.all;
    };
  };
}
