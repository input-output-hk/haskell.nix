{ stdenv, lib, project', haskellLib, recurseIntoAttrs, testSrc, compiler-nix-name, evalPackages }:

with lib;

let
  project = project' {
    inherit compiler-nix-name evalPackages;
    src = testSrc "literate-haskell";
  };

  packages = project.hsPkgs;

in recurseIntoAttrs {
  ifdInputs = {
    inherit (project) plan-nix;
  };

  build = packages.literate-haskell.components.library;
}
