# Test building TH code that needs DLLs when cross compiling for windows
{ stdenv, lib, util, project', haskellLib, testSrc, compiler-nix-name, evalPackages, buildPackages }:

with lib;

let
  project = project' {
    inherit compiler-nix-name evalPackages;
    src = testSrc "exe-dlls";
    cabalProjectLocal = builtins.readFile ../cabal.project.local;
    modules = import ../modules.nix;
  };

  packages = project.hsPkgs;

in lib.recurseIntoAttrs rec {
  meta.disabled = stdenv.hostPlatform.isGhcjs || stdenv.hostPlatform.isWasm;

  ifdInputs = {
    inherit (project) plan-nix;
  };

  build = packages.exe-dlls.components.exes.exe-dlls;
  check = haskellLib.check build;
  build-profiled = packages.exe-dlls.components.exes.exe-dlls.profiled;
  check-profiled = haskellLib.check build-profiled;
}
