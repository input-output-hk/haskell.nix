# A package with a library that ships `data-files`, and a test-suite that
# reads them via `getDataFileName`.  Running the test's `check` exercises that
# the builder makes the package data-dir available to tests — in particular
# under builderVersion = 2, where the check runs the installed binary directly
# and `lib/check.nix` must set `<pkg>_datadir`.
{ lib, project', testSrc, compiler-nix-name, evalPackages }:

let
  mkProject = builderVersion: project' {
    inherit compiler-nix-name evalPackages builderVersion;
    src = testSrc "check-datadir";
  };

  project = mkProject 1;
  projectV2 = mkProject 2;

in lib.recurseIntoAttrs {
  ifdInputs = {
    inherit (project) plan-nix;
    plan-nix-v2 = projectV2.plan-nix;
  };

  run = project.hsPkgs.check-datadir.checks.test;
  run-v2 = projectV2.hsPkgs.check-datadir.checks.test;
}
