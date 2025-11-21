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

  meta.disabled = builtins.elem compiler-nix-name ["ghc91320241204"]
    # Not sure why this is failing with a seg fault
    || (builtins.elem compiler-nix-name ["ghc9102" "ghc9102llvm" "ghc9103" "ghc9103llvm"] && stdenv.hostPlatform.isAndroid && stdenv.hostPlatform.isAarch32)
    # unhandled ELF relocation(Rel) type 10
    || (stdenv.hostPlatform.isMusl && stdenv.hostPlatform.isx86_32)

    # Disable for now (CI machines currently hang without timing out)
    || stdenv.hostPlatform.isWindows || stdenv.hostPlatform.isAndroid
    || (stdenv.buildPlatform.isx86_64 && stdenv.hostPlatform.isAarch64)
    ;

  build = packages.js-template-haskell.components.library;
  check = packages.js-template-haskell.checks.test;
} // optionalAttrs (!(
         stdenv.hostPlatform.isGhcjs
      || (builtins.elem compiler-nix-name ["ghc984" "ghc9122" "ghc9122llvm" "ghc91320250523"] && stdenv.buildPlatform.isx86_64 && stdenv.hostPlatform.isAarch64)
      || (stdenv.hostPlatform.isAarch64
          && stdenv.hostPlatform.isMusl
          && builtins.elem compiler-nix-name ["ghc9101" "ghc966"])
    )) {
  build-profiled = packages.js-template-haskell.components.library.profiled;
  check-profiled = packages.js-template-haskell.checks.test.profiled;
}
