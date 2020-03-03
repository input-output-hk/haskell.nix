# Test a package set
{ stdenv, util, cabalProject', haskellLib, recurseIntoAttrs }:

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
    src = haskellLib.cleanGit { src = ../..; subDir = "test/cabal-simple-prof"; };
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
      exe="${packages.cabal-simple.components.exes.cabal-simple}/bin/cabal-simple${stdenv.hostPlatform.extensions.executable}"

      size=$(command stat --format '%s' "$exe")
      printf "size of executable $exe is $size. \n" >& 2

      # fixme: run on target platform when cross-compiled
      printf "checking whether executable runs with profiling... " >& 2
      ${toString packages.cabal-simple.components.exes.cabal-simple.config.testWrapper} $exe +RTS -p -h

      touch $out
    '';

    meta.platforms = platforms.all;

    passthru = {
      # Used for debugging with nix repl
      inherit project packages;

      # Used for testing externally with nix-shell (../tests.sh).
      # This just adds cabal-install to the existing shells.
      test-shell = util.addCabalInstall packages.cabal-simple.components.all;
    };
  };
}
