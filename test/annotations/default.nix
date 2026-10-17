{ pkgs, stdenv, lib, util, project', haskellLib, testSrc, compiler-nix-name, evalPackages, evalSystem, pkgsBuildBuild }:

with lib;

let
  arm32Qemu = compiler-nix-name == "ghc914-sh"
    && import ../../overlays/qemu-arm32-personality-supported.nix {
    inherit lib;
    inherit (stdenv) buildPlatform hostPlatform;
    qemuVersion = pkgsBuildBuild.qemu.version;
  };
  # Test the repaired emulator without changing the generic native-builder
  # policy: its base restriction also covers older cross-compiler stalls.
  testProject = if arm32Qemu then (pkgs.extend (_: prev: {
    haskell-nix = prev.haskell-nix // { emulatorSystemFeatures = []; };
  })).haskell-nix.project' else project';
  mkProject = profiled: testProject ({
    inherit compiler-nix-name evalSystem;
    src = testSrc "annotations";
    modules = [{
      packages.test-annotations.components.library.usesTemplateHaskell = true;
    }];
  } // lib.optionalAttrs profiled {
    cabalProjectLocal = ''
      package *
        library-profiling: True
    '';
  });
  project = mkProject false;
  profiledProject = mkProject true;

in lib.recurseIntoAttrs ({
  meta.disabled =
    # This fail looking for ghci.  Adding ghci as a `build-depends` works, but should not be needed
    stdenv.hostPlatform.isGhcjs || stdenv.hostPlatform.isWasm
    # Failed to lookup symbol: __aarch64_swp8_acq_rel
    || (builtins.elem compiler-nix-name ["ghc947" "ghc948"] && haskellLib.isCrossHost && stdenv.hostPlatform.isAarch64)
    # unhandled ELF relocation(Rel) type 10
    || (stdenv.hostPlatform.isMusl && stdenv.hostPlatform.isx86_32)
    # Keep the old ARM32 exclusion on routes without the Linux QEMU fix.
    || (stdenv.hostPlatform.isAndroid && stdenv.hostPlatform.isAarch32 && !arm32Qemu)
    # Rosetta error: invalid gdt selector index 5 (wine crashes under Rosetta with msvcrt)
    || (stdenv.hostPlatform.isWindows && stdenv.hostPlatform.libc != "ucrt")
    ;
  ifdInputs = {
    inherit (project) plan-nix;
  } // lib.optionalAttrs arm32Qemu {
    plan-nix-profiled = profiledProject.plan-nix;
  };

  build = project.hsPkgs.test-annotations.components.library;
} // lib.optionalAttrs arm32Qemu {
  build-profiled = profiledProject.hsPkgs.test-annotations.components.library;
})
