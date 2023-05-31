# Test building TH code that needs DLLs when cross compiling for windows
{ stdenv, lib, util, project', haskellLib, recurseIntoAttrs, testSrc, compiler-nix-name, evalPackages }:

with lib;

let
  project = externalInterpreter: project' {
    inherit compiler-nix-name evalPackages;
    src = testSrc "th-dlls";
    modules = [({pkgs, ...}: lib.optionalAttrs externalInterpreter {
      packages.th-dlls.components.library.ghcOptions = [ "-fexternal-interpreter" ];
      # Static openssl seems to fail to load in iserv for musl
      packages.HsOpenSSL.components.library.libs = lib.optional pkgs.stdenv.hostPlatform.isMusl (pkgs.openssl.override { static = false; });
    })];
  };

  packages = (project false).hsPkgs;
  packages-ei = (project true).hsPkgs;

in recurseIntoAttrs {
  meta.disabled = stdenv.hostPlatform.isGhcjs ||
    # TH breaks for ghc 9.4.3 cross compile for windows if the library even
    # just depends on the `text` package (this may be related to the C++ dependency).
    (stdenv.hostPlatform.isWindows && __elem compiler-nix-name ["ghc941" "ghc942" "ghc943" "ghc944" "ghc945" "ghc96020230302" "ghc961" "ghc962"]) ||
    # Similar problem on macOS
    (stdenv.hostPlatform.isDarwin && __elem compiler-nix-name ["ghc941" "ghc942" "ghc943" "ghc944" "ghc945" "ghc96020230302" "ghc961" "ghc962"]) ||
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
