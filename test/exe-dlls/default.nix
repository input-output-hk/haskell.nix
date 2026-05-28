# Test building TH code that needs DLLs when cross compiling for windows
{ stdenv, lib, util, project', haskellLib, testSrc, compiler-nix-name, evalPackages, buildPackages }:

with lib;

let
  project = project' {
    inherit compiler-nix-name evalPackages;
    src = testSrc "exe-dlls";
    cabalProjectLocal = builtins.readFile ../cabal.project.local
      + lib.optionalString stdenv.hostPlatform.isAndroid
          (builtins.readFile ../cabal.project.android);
    modules = import ../modules.nix;
  };

  # `.profiled` is no longer supplied as an overlay rebuild by the
  # v2 builder; profiling is enabled via cabal.project so plan-nix
  # records the matching configure-args (see
  # `docs/dev/profiling.md`).  The profiled variant is a sibling
  # project with the toggles in `cabalProjectLocal`.
  projectProfiled = project' {
    inherit compiler-nix-name evalPackages;
    src = testSrc "exe-dlls";
    cabalProjectLocal = builtins.readFile ../cabal.project.local
      + lib.optionalString stdenv.hostPlatform.isAndroid
          (builtins.readFile ../cabal.project.android)
      + ''
      package *
        library-profiling: True
      package exe-dlls
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
    plan-nix-profiled = projectProfiled.plan-nix;
  };

  build = packages.exe-dlls.components.exes.exe-dlls;
  check = haskellLib.check build;
  build-profiled = packagesProfiled.exe-dlls.components.exes.exe-dlls;
  check-profiled = haskellLib.check build-profiled;
}
