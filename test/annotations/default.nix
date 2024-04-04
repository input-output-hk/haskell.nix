{ stdenv, lib, util, project', haskellLib, recurseIntoAttrs, testSrc, compiler-nix-name, evalPackages }:

with lib;

let
  project = project' {
    inherit compiler-nix-name evalPackages;
    src = testSrc "annotations";
  };

in recurseIntoAttrs {
  meta.disabled = stdenv.hostPlatform.isGhcjs
    # Failed to lookup symbol: __aarch64_swp8_acq_rel
    || (builtins.elem compiler-nix-name ["ghc947" "ghc948"] && haskellLib.isCrossHost && stdenv.hostPlatform.isAarch64);
  ifdInputs = {
    inherit (project) plan-nix;
  };

  build = project.hsPkgs.test-annotations.components.library;
}
