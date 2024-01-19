{ stdenv, lib, util, project', haskellLib, recurseIntoAttrs, testSrc, compiler-nix-name, evalPackages }:

with lib;

let
  project = project' {
    inherit compiler-nix-name evalPackages;
    src = testSrc "annotations";
  };

in recurseIntoAttrs {
  ifdInputs = {
    inherit (project) plan-nix;
  };

  build = project.hsPkgs.test-annotations.components.library;
}
