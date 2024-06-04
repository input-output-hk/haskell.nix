# Test building TH code that needs DLLs when cross compiling for windows
{ stdenv, lib, util, project', haskellLib, recurseIntoAttrs, testSrc, compiler-nix-name, evalPackages, buildPackages }:

with lib;

let
  project = externalInterpreter: project' {
    inherit compiler-nix-name evalPackages;
    src = testSrc "th-dlls-minimal";
    cabalProjectLocal = builtins.readFile ../cabal.project.local;
    modules = [
     ({pkgs, ...}: {
      packages.test-lib.components.library.libs = mkForce [
        (pkgs.stdenv.mkDerivation {
          name = "test-clib";
          version = "1.0";
          src = testSrc "th-dlls-minimal/test-clib";
        })
      ];
     })
     ({pkgs, ...}: lib.optionalAttrs externalInterpreter {
      packages.th-dlls-minimal.components.library.ghcOptions = [ "-fexternal-interpreter" ];
    })];
  };

  packages = (project false).hsPkgs;
  packages-ei = (project true).hsPkgs;

in recurseIntoAttrs {
  # This test is just for windows currently (the full th-dlls test runs on other platforms)
  meta.disabled = !stdenv.hostPlatform.isWindows;

  ifdInputs = {
    inherit (project true) plan-nix;
  };

  build = packages.th-dlls-minimal.components.library;
  build-profiled = packages.th-dlls-minimal.components.library.profiled;
  just-template-haskell = packages.th-dlls-minimal.components.exes.just-template-haskell;
  build-ei = packages-ei.th-dlls-minimal.components.library;
  build-profiled-ei = packages-ei.th-dlls-minimal.components.library.profiled;
  just-template-haskell-ei = packages-ei.th-dlls-minimal.components.exes.just-template-haskell;
}
