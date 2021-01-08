# Test a package set
{ stdenv, util, cabalProject', haskellLib, recurseIntoAttrs, testSrc, compiler-nix-name }:

with stdenv.lib;

let
  modules = [
    {
      # Package has no exposed modules which causes
      #   haddock: No input file(s)
      packages.cabal-simple.doHaddock = false;
      packages.cabal-simple.enableExecutableProfiling = true;
      enableLibraryProfiling = true;
      # executableProfiling = false;
    }
  ];

  project = cabalProject' {
    inherit compiler-nix-name;
    src = testSrc "cabal-simple-prof";
    inherit modules;
  };

  packages = project.hsPkgs;

in recurseIntoAttrs {
  ifdInputs = {
    inherit (project) plan-nix;
  };
  run = stdenv.mkDerivation {
    name = "cabal-simple-prof-test";

    buildCommand = ''
      exe="${packages.cabal-simple.components.exes.cabal-simple.exePath}"

      size=$(command stat --format '%s' "$exe")
      printf "size of executable $exe is $size. \n" >& 2

      # fixme: run on target platform when cross-compiled
      printf "checking whether executable runs with profiling... " >& 2
      # Curiosity: cross compilers prodcing profiling with `+RTS -p -h` lead to the following cryptic message:
      #   cabal-simple: invalid heap profile option: -h*
      # Hence we pass `-hc`.
      ${toString packages.cabal-simple.components.exes.cabal-simple.config.testWrapper} $exe +RTS -p -hc

      touch $out
    '';

    meta = {
      platforms = platforms.all;
      # This test seeems to be broken on 8.6 and 8.8
      disabled = compiler-nix-name == "ghc865" || compiler-nix-name == "ghc884";
    };

    passthru = {
      # Used for debugging with nix repl
      inherit project packages;
    };
  };
}
