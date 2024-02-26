{ stdenv, lib, haskellLib, project', recurseIntoAttrs, testSrc, compiler-nix-name, evalPackages, buildPackages }:

with lib;

let
  project = project' {
    inherit compiler-nix-name evalPackages;
    src = testSrc "plugin";
    cabalProjectLocal = builtins.readFile ../cabal.project.local;
    modules = [{
      reinstallableLibGhc = builtins.compareVersions buildPackages.haskell-nix.compiler.${compiler-nix-name}.version "9.8.1" < 0;
    }];
  };

  packages = project.hsPkgs;

in recurseIntoAttrs {
  ifdInputs = {
    inherit (project) plan-nix;
  };

  # Not sure why this breaks for ghc 8.10.7
  meta.disabled = compiler-nix-name == "ghc8107"
    || compiler-nix-name == "ghc9920240225" # undefined symbol: hs_atomics_primops_store_load_barrier
    || stdenv.hostPlatform.isMusl
    || stdenv.hostPlatform.isGhcjs
    || stdenv.hostPlatform.isWindows
    || (haskellLib.isCrossHost && stdenv.hostPlatform.isAarch64);
  build = packages.test.components.library;
}
