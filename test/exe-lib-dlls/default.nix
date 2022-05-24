# Test building TH code that needs DLLs when cross compiling for windows
{ stdenv, lib, util, project', haskellLib, recurseIntoAttrs, testSrc, compiler-nix-name }:

with lib;

let
  project = project' {
    inherit compiler-nix-name;
    src = testSrc "exe-lib-dlls";
  };

  packages = project.hsPkgs;

in recurseIntoAttrs rec {
  meta.disabled = stdenv.hostPlatform.isGhcjs;

  ifdInputs = {
    inherit (project) plan-nix;
  };

  build = packages.exe-lib-dlls.components.exes.exe-lib-dlls;
  check = haskellLib.check build;
  build-profiled = packages.exe-lib-dlls.components.exes.exe-lib-dlls.profiled;
  check-profiled = haskellLib.check build-profiled;
}
