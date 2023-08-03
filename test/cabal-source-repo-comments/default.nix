{ stdenv, lib, cabalProject', recurseIntoAttrs, haskellLib, testSrc, compiler-nix-name, evalPackages }:

with lib;

let
  project = cabalProject' {
    inherit compiler-nix-name evalPackages;
    src = testSrc "cabal-source-repo-comments";
    cabalProjectLocal = lib.optionalString (__elem compiler-nix-name ["ghc9820230704"]) ''
      source-repository-package
        type: git
        location: https://github.com/glguy/th-abstraction.git
        tag: 24b9ea9b498b182e44abeb3a755e2b4e35c48788
        --sha256: sha256-nWWZVEek0fNVRI+P5oXkuJyrPJWts5tCphymFoYWIPg=
    '';
  };
  packages = project.hsPkgs;
in recurseIntoAttrs {
  ifdInputs = {
    inherit (project) plan-nix;
  };
  run = stdenv.mkDerivation {
    name = "cabal-source-repo-comments-test";

    buildCommand = ''
      exe="${packages.use-cabal-simple.components.exes.use-cabal-simple.exePath}"

      printf "checking whether executable runs... " >& 2
      cat ${haskellLib.check packages.use-cabal-simple.components.exes.use-cabal-simple}/test-stdout

      touch $out
    '';

    meta = rec {
      platforms = lib.platforms.all;
      broken = stdenv.hostPlatform.isGhcjs && __elem compiler-nix-name ["ghc961" "ghc962" "ghc9820230704"];
      disabled = broken;
    };
  
    passthru = {
      # Attributes used for debugging with nix repl
      inherit packages;
    };
  };
}
