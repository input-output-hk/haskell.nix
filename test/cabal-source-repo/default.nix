{ stdenv, lib, cabalProject', recurseIntoAttrs, haskellLib, testSrc, compiler-nix-name, evalPackages, buildPackages }:

with lib;

let
  project = cabalProject' {
    inherit compiler-nix-name evalPackages;
    src = testSrc "cabal-source-repo";
    cabalProjectLocal = builtins.readFile ../cabal.project.local;
  };
  packages = project.hsPkgs;
in recurseIntoAttrs {
  ifdInputs = {
    inherit (project) plan-nix;
  };
  run = stdenv.mkDerivation {
    name = "cabal-source-repo-test";

    buildCommand = ''
      exe="${packages.use-cabal-simple.components.exes.use-cabal-simple.exePath}"

      printf "checking whether executable runs... " >& 2
      cat ${haskellLib.check packages.use-cabal-simple.components.exes.use-cabal-simple}/test-stdout

      touch $out
    '';

    meta = rec {
      platforms = lib.platforms.all;
      broken = stdenv.hostPlatform.isGhcjs && __compareVersions buildPackages.haskell-nix.compiler.${compiler-nix-name}.version "9.6.1" >= 0;
      disabled = broken;
    };

    passthru = {
      # Attributes used for debugging with nix repl
      inherit packages;
    };
  };
}
