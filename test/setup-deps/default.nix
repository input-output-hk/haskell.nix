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

  meta = {
    platforms = platforms.unix;
    # Building reinstallable lib GHC is broken on 8.10, and we require lib ghc so this won't work with cross-compiling.
    # Moreover, even building the plan doesn't seem to work in these circumstances.
    disabled = stdenv.buildPlatform != stdenv.hostPlatform || stdenv.hostPlatform.isMusl || __elem compiler-nix-name ["ghc8101" "ghc8102" "ghc8103" "ghc8104" "ghc8105" "ghc8106" "ghc8107" "ghc810420210212"];
  };
in 

recurseIntoAttrs ({
  meta.disabled = __elem compiler-nix-name ["ghc901" "ghc902" "ghc921" "ghc922"];

  ifdInputs = {
    plan-nix = addMetaAttrs meta project.plan-nix;
  };
  run = pkgs.stdenv.mkDerivation {
    name = "setup-deps-test";

    buildCommand = ''
      exe="${project.getComponent "pkg:exe:pkg"}/bin/pkg"

      printf "checking whether executable runs... " >& 2
      $exe

      touch $out
    '';

    inherit meta;
    passthru = {
      # Attributes used for debugging with nix repl
      inherit project;
    };
  };
})
