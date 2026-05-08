# Test building TH code that needs DLLs when cross compiling for windows
{ stdenv, lib, util, project', haskellLib, testSrc, compiler-nix-name, evalPackages }:

with lib;

let
  project = project' {
    inherit compiler-nix-name evalPackages;
    src = testSrc "exe-lib-dlls";
    cabalProjectLocal = builtins.readFile ../cabal.project.local;
    modules = import ../modules.nix;
  };

  # See `docs/dev/profiling.md` — v2 expects profiling toggles to
  # come from cabal.project so plan-nix records them.
  projectProfiled = project' {
    inherit compiler-nix-name evalPackages;
    src = testSrc "exe-lib-dlls";
    cabalProjectLocal = builtins.readFile ../cabal.project.local + ''
      package *
        library-profiling: True
      package exe-lib-dlls
        profiling: True
    '';
    modules = import ../modules.nix;
  };

  packages = project.hsPkgs;
  packagesProfiled = projectProfiled.hsPkgs;

in lib.recurseIntoAttrs rec {
  meta.disabled = stdenv.hostPlatform.isGhcjs || stdenv.hostPlatform.isWasm;

  ifdInputs = {
    inherit (project) plan-nix;
  };

  build = packages.exe-lib-dlls.components.exes.exe-lib-dlls;
  check = haskellLib.check build;
  build-profiled = packagesProfiled.exe-lib-dlls.components.exes.exe-lib-dlls;
  check-profiled = haskellLib.check build-profiled;
}
