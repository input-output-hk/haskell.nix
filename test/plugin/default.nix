{ stdenv, lib, haskellLib, project', testSrc, compiler-nix-name, evalPackages, buildPackages }:

with lib;

let
  project = project' {
    inherit compiler-nix-name evalPackages;
    src = testSrc "plugin";
    cabalProjectLocal = builtins.readFile ../cabal.project.local + ''
      allow-newer: polysemy-plugin:containers, polysemy:containers
    '';
  };

  packages = project.hsPkgs;

in lib.recurseIntoAttrs {
  ifdInputs = {
    inherit (project) plan-nix;
  };

  meta.disabled =
       builtins.elem compiler-nix-name [ "ghc91320250523" ]
    || stdenv.hostPlatform.isMusl
    || stdenv.hostPlatform.isGhcjs
    || stdenv.hostPlatform.isWasm
    || stdenv.hostPlatform.isWindows
    || (haskellLib.isCrossHost && (stdenv.hostPlatform.isAarch64 || stdenv.hostPlatform.isAarch32));
  build = packages.test.components.library;
}
