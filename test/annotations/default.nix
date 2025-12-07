{ stdenv, lib, util, project', haskellLib, testSrc, compiler-nix-name, evalPackages }:

with lib;

let
  project = project' {
    inherit compiler-nix-name evalPackages;
    src = testSrc "annotations";
  };

in lib.recurseIntoAttrs {
  meta.disabled = 
    # This fail looking for ghci.  Adding ghci as a `build-depends` works, but should not be needed
    stdenv.hostPlatform.isGhcjs || stdenv.hostPlatform.isWasm
    # Failed to lookup symbol: __aarch64_swp8_acq_rel
    || (builtins.elem compiler-nix-name ["ghc947" "ghc948"] && haskellLib.isCrossHost && stdenv.hostPlatform.isAarch64)
    # unhandled ELF relocation(Rel) type 10
    || (stdenv.hostPlatform.isMusl && stdenv.hostPlatform.isx86_32)
    # Disable for now (CI machines currently timing out)
    || stdenv.hostPlatform.isWindows || stdenv.hostPlatform.isAndroid
    || (stdenv.buildPlatform.isx86_64 && stdenv.hostPlatform.isAarch64)
    ;
  ifdInputs = {
    inherit (project) plan-nix;
  };

  build = project.hsPkgs.test-annotations.components.library;
}
