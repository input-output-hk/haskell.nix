# See https://github.com/input-output-hk/haskell.nix/issues/2228
{ stdenv, lib, util, project', haskellLib, recurseIntoAttrs, testSrc, compiler-nix-name, evalPackages, buildPackages }:

with lib;

let
  project = project' {
    inherit compiler-nix-name evalPackages;
    src = testSrc "src-loc";
  };

  packages = project.hsPkgs;

in recurseIntoAttrs rec {
  meta.disabled = stdenv.hostPlatform.isGhcjs;

  ifdInputs = {
    inherit (project) plan-nix;
  };

  build = packages.src-loc.components.exes.src-loc;
  check = haskellLib.check build;
  build-profiled = packages.src-loc.components.exes.src-loc.profiled;
  check-profiled = haskellLib.check build-profiled;
}
