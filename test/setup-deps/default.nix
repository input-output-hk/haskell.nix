{ pkgs }:

with pkgs;
with stdenv.lib;

let
  project = haskell-nix.cabalProject' {
    src = pkgs.haskell-nix.haskellLib.cleanGit { src = ../..; subDir = "test/setup-deps"; };
    modules = [{
      # Package has no exposed modules which causes
      #   haddock: No input file(s)
      packages.bytestring-builder.doHaddock = false;
    }
    { reinstallableLibGhc = true; } ];
  };

  packages = project.hsPkgs;
in recurseIntoAttrs (if stdenv.hostPlatform.isWindows
 then
    let skip = pkgs.runCommand "skip-test-setup-deps" {} ''
      echo "Skipping setup-deps test on windows as it needs the ghc lib" >& 2
      touch $out
    '';
    in {
      ifdInputs = { plan-nix = skip; };
      run = skip;
    }
 else {
  ifdInputs = {
    inherit (project) plan-nix;
  };
  run = pkgs.stdenv.mkDerivation {
    name = "setup-deps-test";

    buildCommand = ''
      exe="${packages.pkg.components.exes.pkg}/bin/pkg"

      printf "checking whether executable runs... " >& 2
      $exe

      touch $out
    '';

    meta.platforms = platforms.unix;
    meta.disabled = stdenv.hostPlatform.isMusl || stdenv.hostPlatform.isWindows;

    passthru = {
      # Attributes used for debugging with nix repl
      inherit project packages;
    };
  };
})
