# Test building TH code that needs DLLs when cross compiling for windows
{ stdenv, lib, util, project', haskellLib, recurseIntoAttrs, testSrc, compiler-nix-name, evalPackages, buildPackages }:

with lib;

let
  project = externalInterpreter: project' {
    inherit compiler-nix-name evalPackages;
    src = testSrc "th-dlls";
    cabalProjectLocal = builtins.readFile ../cabal.project.local;
    modules = [({pkgs, ...}: lib.optionalAttrs externalInterpreter {
      packages.th-dlls.components.library.ghcOptions = [ "-fexternal-interpreter" ];
      # Static openssl seems to fail to load in iserv for musl
      packages.HsOpenSSL.components.library.libs = lib.optional pkgs.stdenv.hostPlatform.isMusl (pkgs.openssl.override { static = false; });
    })];
  };

  packages = (project false).hsPkgs;
  packages-ei = (project true).hsPkgs;

in recurseIntoAttrs {
  meta.disabled = stdenv.hostPlatform.isGhcjs
    # On aarch64 this test also breaks form musl builds (including cross compiles on x86_64-linux)
    || (stdenv.hostPlatform.isAarch64 && stdenv.hostPlatform.isMusl)
    # Failed to lookup symbol: __aarch64_swp8_acq_rel
    || (builtins.elem compiler-nix-name ["ghc947" "ghc948"] && haskellLib.isCrossHost && stdenv.hostPlatform.isAarch64)
    # We have been unable to get windows cross compilation of th-orphans to work for GHC 8.10 using the latest nixpkgs
    || (compiler-nix-name == "ghc8107" && stdenv.hostPlatform.isWindows)
    # We need to update GHC HEAD to get a version of ghc-internal compatible
    # with th-lift from head.hackage.
    || builtins.elem compiler-nix-name [ "ghc91220241014" "ghc91320241101" ]
    ;

  ifdInputs = {
    inherit (project true) plan-nix;
  };

  build = packages.th-dlls.components.library;
  build-profiled = packages.th-dlls.components.library.profiled;
  just-template-haskell = packages.th-dlls.components.exes.just-template-haskell;
  build-ei = packages-ei.th-dlls.components.library;
  build-profiled-ei = packages-ei.th-dlls.components.library.profiled;
  just-template-haskell-ei = packages-ei.th-dlls.components.exes.just-template-haskell;
}
