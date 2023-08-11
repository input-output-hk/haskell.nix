# Test building TH code that needs DLLs when cross compiling for windows
{ stdenv, lib, util, project', haskellLib, recurseIntoAttrs, testSrc, compiler-nix-name, evalPackages, buildPackages }:

with lib;

let
  project = project' {
    inherit compiler-nix-name evalPackages;
    src = testSrc "exe-dlls";
    cabalProjectLocal = lib.optionalString (__compareVersions buildPackages.haskell-nix.compiler.${compiler-nix-name}.version "9.8.0" >= 0) ''
      allow-newer: *:*
    '';
  };

  packages = project.hsPkgs;

in recurseIntoAttrs rec {
  meta.disabled = stdenv.hostPlatform.isGhcjs;

  ifdInputs = {
    inherit (project) plan-nix;
  };

  build = packages.exe-dlls.components.exes.exe-dlls;
  check = haskellLib.check build;
  build-profiled = packages.exe-dlls.components.exes.exe-dlls.profiled;
  check-profiled = haskellLib.check build-profiled;
}
