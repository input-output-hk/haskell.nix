{ pkgs }:

with pkgs;
with stdenv.lib;

let
  project = haskell-nix.cabalProject' {
    name = "test-setup-deps";
    src = pkgs.haskell-nix.haskellLib.cleanGit { src = ../..; subDir = "test/setup-deps"; };
    modules = [{
      # Package has no exposed modules which causes
      #   haddock: No input file(s)
      packages.bytestring-builder.doHaddock = false;
    }
    { reinstallableLibGhc = true; } ];
  };

  packages = project.hsPkgs;
in recurseIntoAttrs {
  plan = haskell-nix.withInputs project.plan-nix;
  run = pkgs.stdenv.mkDerivation {
    name = "call-cabal-project-to-nix-test";

    buildCommand = ''
      exe="${packages.pkg.components.exes.pkg}/bin/pkg"

      printf "checking whether executable runs... " >& 2
      $exe

      touch $out
    '';

    meta.platforms = platforms.all;

    passthru = {
      # Attributes used for debugging with nix repl
      inherit project packages;
    };
  };
}
