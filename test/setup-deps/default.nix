{ pkgs, compiler-nix-name }:

with pkgs;
with lib;

let
  project = haskell-nix.cabalProject' {
    inherit compiler-nix-name;
    src = pkgs.haskell-nix.haskellLib.cleanGit { src = ../..; name = "setup-deps"; subDir = "test/setup-deps"; };
    modules = [{
      # Package has no exposed modules which causes
      #   haddock: No input file(s)
      packages.bytestring-builder.doHaddock = false;
    }
    { reinstallableLibGhc = true; } ];
  };

  packages = project.hsPkgs;
  meta = {
    platforms = platforms.unix;
    # Building reinstallable lib GHC is broken on 8.10, and we require lib ghc so this won't work with cross-compiling.
    # Moreover, even building the plan doesn't seem to work in these circumstances.
    disabled = stdenv.buildPlatform != stdenv.hostPlatform || stdenv.hostPlatform.isMusl || compiler-nix-name == "ghc8102" || compiler-nix-name == "ghc8103";
  };
in 

recurseIntoAttrs ({
  ifdInputs = {
    plan-nix = addMetaAttrs meta project.plan-nix;
  };
  run = pkgs.stdenv.mkDerivation {
    name = "setup-deps-test";

    buildCommand = ''
      exe="${packages.pkg.components.exes.pkg}/bin/pkg"

      printf "checking whether executable runs... " >& 2
      $exe

      touch $out
    '';

    inherit meta;
    passthru = {
      # Attributes used for debugging with nix repl
      inherit project packages;
    };
  };
})
