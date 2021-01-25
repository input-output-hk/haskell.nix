# Test a package set
{ stdenv, util, cabalProject', haskellLib, gmp6, zlib, recurseIntoAttrs, runCommand, testSrc, compiler-nix-name }:

with stdenv.lib;

let
  project =
    cabalProject' {
      inherit compiler-nix-name;
      src = testSrc "cabal-doctests";
      index-state = "2021-01-11T00:00:00Z";
    };

  packages = project.hsPkgs;

  meta = {
    platforms = platforms.all;
    # Making this work for cross compilers will be difficult.
    disabled = stdenv.buildPlatform != stdenv.hostPlatform;
  };

in recurseIntoAttrs ({
  ifdInputs = {
    plan-nix = addMetaAttrs meta project.plan-nix;
  };
  run = stdenv.mkDerivation {
    name = "cabal-doctests-test";

    buildCommand = ''
      printf "Checking that doctest tests have run ... " >& 2
      cat ${packages.cabal-doctests-test.checks.doctests}/test-stdout >& 2

      touch $out
    '';

    passthru = {
      # Used for debugging with nix repl
      inherit project packages;
    };
  };
})
