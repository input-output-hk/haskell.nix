# Test building TH code that needs DLLs when cross compiling for windows
{ stdenv, lib, util, project', haskellLib, recurseIntoAttrs, testSrc, compiler-nix-name, evalPackages, buildPackages }:

with lib;

let
  project = externalInterpreter: project' {
    inherit compiler-nix-name evalPackages;
    src = testSrc "th-dlls";
    cabalProjectLocal = builtins.readFile ../cabal.project.local;
    modules = import ../modules.nix ++ [({pkgs, ...}: lib.optionalAttrs externalInterpreter {
      packages.th-dlls.components.library.ghcOptions = [ "-fexternal-interpreter" ];
      # Static openssl seems to fail to load in iserv for musl
      packages.HsOpenSSL.components.library.libs = lib.optional pkgs.stdenv.hostPlatform.isMusl (pkgs.openssl.override { static = false; });
    })];
  };

  packages = (project false).hsPkgs;
  packages-ei = (project true).hsPkgs;

in recurseIntoAttrs {
  meta.disabled = stdenv.hostPlatform.isGhcjs
    # Not sure why this is failing with a seg fault
    || (builtins.elem compiler-nix-name ["ghc9102" "ghc9102llvm"] && stdenv.hostPlatform.isAndroid && stdenv.hostPlatform.isAarch32)
    # unhandled ELF relocation(Rel) type 10
    || (stdenv.hostPlatform.isMusl && stdenv.hostPlatform.isx86_32)

    ## Old GHC versions (TODO remove)
    # Failed to lookup symbol: __aarch64_swp8_acq_rel
    || (builtins.elem compiler-nix-name ["ghc947" "ghc948"] && haskellLib.isCrossHost && stdenv.hostPlatform.isAarch64)
    # We have been unable to get windows cross compilation of th-orphans to work for GHC 8.10 using the latest nixpkgs
    || (compiler-nix-name == "ghc8107" && stdenv.hostPlatform.isWindows)
    ;

  ifdInputs = {
    inherit (project true) plan-nix;
  };

  build = packages.th-dlls.components.library;
  just-template-haskell = packages.th-dlls.components.exes.just-template-haskell;
  build-ei = packages-ei.th-dlls.components.library;
  just-template-haskell-ei = packages-ei.th-dlls.components.exes.just-template-haskell;
} // optionalAttrs
    (!(builtins.elem compiler-nix-name ["ghc984" "ghc9122" "ghc9122llvm" "ghc91320250523"]  && stdenv.buildPlatform.isx86_64 && stdenv.hostPlatform.isAarch64
      # The dependency on `math-functions` somehow breaks GHC 9.6.7 musl profiled builds (only with the external interpreter though)
      || (compiler-nix-name == "ghc967" && stdenv.hostPlatform.isMusl && stdenv.hostPlatform.isx86_64))) {
  # On for aarch64 cross compile on GHC this test is fails sometimes for non profiled builds
  # (and always for the profiled builds).
  # This may be related to the memory allocation changes made in 9.8.4 that
  # replace the pool allocator patches we used in earlier versions.

  # Interestingly GHC 9.10.1 and HEAD are wotking while 9.8.4 and 9.12 seem break.
  # Perhaps there is a fix in GHC HEAD?
  build-profiled = packages.th-dlls.components.library.profiled;
  build-profiled-ei = packages-ei.th-dlls.components.library.profiled;
}
