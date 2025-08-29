{ stdenv, pkgs, lib, recurseIntoAttrs, testSrc, compiler-nix-name, evalPackages, buildPackages }:

let
  ghc = buildPackages.haskell-nix.compiler.${compiler-nix-name}.override { toolEvalPackages = evalPackages; };

  supported-langauges = import ../../lib/supported-languages.nix {
    inherit pkgs evalPackages ghc;
  };

in recurseIntoAttrs {
  check = stdenv.mkDerivation {
    name = "support-languages-check";

    buildCommand = ''
      expected=$(mktemp)
      dummy=$(mktemp)
      ${ghc}/bin/${ghc.targetPrefix}ghc --supported-languages | sort >$expected
      sort ${supported-langauges} > $dummy

      echo 'Expected `${ghc}/bin/${ghc.targetPrefix}ghc --supported-languages | sort`'
      echo 'The calculated by lib/supported-languages.nix `sort ${supported-langauges}`'
      echo 'diff -u $dummy $expected'
      diff -u $dummy $expected

      touch $out
    '';

    meta = rec {
      platforms = lib.platforms.all;
    };
  };
}
