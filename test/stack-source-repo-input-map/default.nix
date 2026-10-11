{ lib, stdenv, stackProject', testSrc, compiler-nix-name, evalPackages }:

let
  project = stackProject' {
    src = testSrc "stack-source-repo-input-map";
    inherit evalPackages;
    # Keyed by "<url>/<rev>", so the value can be a plain source tree.
    inputMap."https://github.com/input-output-hk/haskell.nix-does-not-exist.git/0123456789abcdef0123456789abcdef01234567" =
      testSrc "cabal-simple";
  };
  packages = project.hsPkgs;

in lib.recurseIntoAttrs {
  meta.disabled = compiler-nix-name != "ghc984" || stdenv.hostPlatform.isGhcjs;
  ifdInputs = {
    inherit (project) stack-nix;
  };
  inherit (packages.stack-source-repo-input-map.components) library;
}
