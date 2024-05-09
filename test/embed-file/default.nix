# Test that embedFile function for the file-embed package works
{ stdenv, lib, project', haskellLib, recurseIntoAttrs, testSrc, compiler-nix-name, evalPackages, buildPackages }:

with lib;

let
  project = project' {
    inherit compiler-nix-name evalPackages;
    src = testSrc "embed-file";
    # cabalProjectLocal = builtins.readFile ../cabal.project.local;
  };

  packages = project.hsPkgs;

in recurseIntoAttrs rec {
  meta.disabled = stdenv.hostPlatform.isGhcjs;

  ifdInputs = {
    inherit (project) plan-nix;
  };

  build = packages.embed-file.components.exes.embed-file;
  check = haskellLib.check build;
  # build-profiled = packages.embed-file.components.exes.embed-file.profiled;
  # check-profiled = haskellLib.check build-profiled;
}
