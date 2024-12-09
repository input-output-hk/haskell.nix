# Test building TH code that needs DLLs when cross compiling for windows
{ stdenv, lib, project', haskellLib, recurseIntoAttrs, testSrc, compiler-nix-name, evalPackages }:

with lib;

let
  project = project' {
    inherit compiler-nix-name evalPackages;
    src = testSrc "js-template-haskell";
    cabalProjectLocal = builtins.readFile ../cabal.project.local
      + ''
      if arch(javascript)
        extra-packages: ghci
        constraints: ghcjs installed
      constraints: text -simdutf, text source
    '';
  };

  packages = project.hsPkgs;

in recurseIntoAttrs {
  ifdInputs = {
    inherit (project) plan-nix;
  };

  meta.disabled = stdenv.buildPlatform != stdenv.hostPlatform && stdenv.hostPlatform.isAarch64
    || builtins.elem compiler-nix-name ["ghc91320241204"];

  build = packages.js-template-haskell.components.library;
  check = packages.js-template-haskell.checks.test;
} // optionalAttrs (!stdenv.hostPlatform.isGhcjs) {
  build-profiled = packages.js-template-haskell.components.library.profiled;
  check-profiled = packages.js-template-haskell.checks.test.profiled;
}
