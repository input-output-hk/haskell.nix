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
  compareGhc = builtins.compareVersions buildPackages.haskell-nix.compiler.${compiler-nix-name}.version;

in recurseIntoAttrs {
  meta.disabled = stdenv.hostPlatform.isGhcjs ||
    # We have added patches to help loading DLLs for TH windows cross compilation.
    # These are working for GHC 9.6.2, but changes in 9.4.7 (released after 9.6.2)
    # and the current git ghc-9.8 and head branches result in similar issues.
    (stdenv.hostPlatform.isWindows && (
      (compareGhc "9.4.0" >= 0 && compareGhc "9.6" < 0) ||
      (compareGhc "9.8.0" >= 0))) ||
    # the macOS linker tries to load `clang++` :facepalm:
    (stdenv.hostPlatform.isDarwin && compareGhc "9.4.0" >= 0) ||
    # On aarch64 this test also breaks form musl builds (including cross compiles on x86_64-linux)
    (stdenv.hostPlatform.isAarch64 && stdenv.hostPlatform.isMusl);

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
