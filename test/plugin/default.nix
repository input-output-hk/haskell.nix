{ stdenv, lib, project', recurseIntoAttrs, testSrc, compiler-nix-name, evalPackages }:

with lib;

let
  project = project' {
    inherit compiler-nix-name evalPackages;
    src = testSrc "plugin";
  };

  packages = project.hsPkgs;

in recurseIntoAttrs {
  ifdInputs = {
    inherit (project) plan-nix;
  };

  build = packages.test.components.library;
}
