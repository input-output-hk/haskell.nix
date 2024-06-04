# Test building TH code that needs DLLs when cross compiling for windows
{ stdenv, lib, project', haskellLib, recurseIntoAttrs, testSrc, compiler-nix-name, ... }:

with lib;

let
  project = project' {
    inherit compiler-nix-name;
    src = testSrc "js-template-haskell";
    cabalProjectLocal = ''
      if arch(javascript)
        if impl(ghc>9)
          extra-packages: ghci
    '';
  };

  packages = project.hsPkgs;

in recurseIntoAttrs {
  ifdInputs = {
    inherit (project) plan-nix;
  };

  meta.disabled = stdenv.buildPlatform != stdenv.hostPlatform && stdenv.hostPlatform.isAarch64;

  build = packages.js-template-haskell.components.library;
  check = packages.js-template-haskell.checks.test;
} // optionalAttrs (!stdenv.hostPlatform.isGhcjs) {
  build-profiled = packages.js-template-haskell.components.library.profiled;
  check-profiled = packages.js-template-haskell.checks.test.profiled;
}
