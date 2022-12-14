# Test building TH code that needs DLLs when cross compiling for windows
{ stdenv, lib, util, project', haskellLib, recurseIntoAttrs, testSrc, compiler-nix-name, evalPackages }:

with lib;

let
  project = project' {
    inherit compiler-nix-name evalPackages;
    src = testSrc "th-dlls";
  };

  packages = project.hsPkgs;

in recurseIntoAttrs {
  meta.disabled = stdenv.hostPlatform.isGhcjs ||
    # TH breaks for ghc 9.4.3 cross compile for windows if the library even
    # just depends on the `text` package (might be related to the C++ dependency).
    (stdenv.hostPlatform.isWindows && __elem compiler-nix-name ["ghc941" "ghc942" "ghc943"]);

  ifdInputs = {
    inherit (project) plan-nix;
  };

  build = packages.th-dlls.components.library;
  build-profiled = packages.th-dlls.components.library.profiled;
  just-template-haskell = packages.th-dlls.components.exes.just-template-haskell;
}
