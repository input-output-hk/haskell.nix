# Test a package set
{ stdenv, util, cabalProject', haskellLib, gmp6, zlib, recurseIntoAttrs }:

with stdenv.lib;

let
  project =
    cabalProject' {
      src = haskellLib.cleanGit { src = ../..; subDir = "test/cabal-doctests"; };
      index-state = "2020-01-26T00:00:00Z";
      modules = [
        {
          # This is needed for the doctest library to be built.  Without it, you
          # get an error like the following:
          #
          # Configuring library for doctest-0.16.2..
          # Error:
          #     The following packages are broken because other packages they
          #     depend on are missing. These broken packages must be rebuilt
          #     before they can be used.
          #     - installed package ghc-8.6.5 is broken due to missing package
          #       terminfo-0.4.1.2
          nonReinstallablePkgs =
            [ "rts" "ghc-heap" "ghc-prim" "integer-gmp" "integer-simple" "base"
              "deepseq" "array" "ghc-boot-th" "pretty" "template-haskell"
              "ghc-boot"
              "ghc" "Cabal" "Win32" "array" "binary" "bytestring" "containers"
              "directory" "filepath" "ghc-boot" "ghc-compact" "ghc-prim"
              "ghci" "haskeline"
              "hpc"
              "mtl" "parsec" "process" "text" "time" "transformers"
              "unix" "xhtml"
              "stm" "terminfo"
            ];

          packages.cabal-doctests-test.components.tests.doctests.isDoctest = true;
        }
      ];
    };

  packages = project.hsPkgs;

in

recurseIntoAttrs {
  ifdInputs = {
    inherit (project) plan-nix;
  };

  # Used for testing externally with nix-shell (../tests.sh).
  # This just adds cabal-install to the existing shells.
  # test-shell = util.addCabalInstall packages.cabal-doctests-test.components.all;

  run = stdenv.mkDerivation {
    name = "cabal-doctests-test";

    buildCommand = ''
      printf "Checking that doctest tests have run ... " >& 2
      doctest_output="${packages.cabal-doctests-test.checks.doctests}"
      test -f "$doctest_output"
      cat "$doctest_output" >& 2

      touch $out
    '';

    meta.platforms = platforms.all;

    passthru = {
      # Used for debugging with nix repl
      inherit project packages;
    };
  };
}
