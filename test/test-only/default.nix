{ stdenv, lib, util, project', haskellLib, testSrc, compiler-nix-name, evalPackages }:

with lib;

let
  project = project' {
    inherit compiler-nix-name evalPackages;
    src = testSrc "test-only";
  };

in lib.recurseIntoAttrs {
  ifdInputs = {
    inherit (project) plan-nix;
  };

  run = project.hsPkgs.test-only.checks.my-test;
}
