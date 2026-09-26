{ stdenv, lib, util, project', haskellLib, testSrc, compiler-nix-name, evalPackages, evalSystem }:

with lib;

let
  project = project' {
    inherit compiler-nix-name evalSystem;
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
    # qemu-arm segfaults running iserv-proxy-interpreter when ghc
    # spawns iserv for the annotations TH eval (the aarch64-android
    # qemu-aarch64 path doesn't hit this).
    || (stdenv.hostPlatform.isAndroid && stdenv.hostPlatform.isAarch32)
    # Rosetta error: invalid gdt selector index 5 (wine crashes under Rosetta with msvcrt)
    || (stdenv.hostPlatform.isWindows && stdenv.hostPlatform.libc != "ucrt")
    ;
  ifdInputs = {
    inherit (project) plan-nix;
  };

  build = project.hsPkgs.test-annotations.components.library;
}
