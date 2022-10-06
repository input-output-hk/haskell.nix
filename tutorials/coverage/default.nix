# default.nix
{ pkgs ? import <nixpkgs> {}}:
let
  inherit (pkgs.haskell-nix) haskellLib;

  project = haskellLib.project {
    src = pkgs.haskell-nix.haskellLib.cleanGit {
      name = "haskell-nix-project";
      src = ./.;
    };
    compiler-nix-name = "ghc884";

    modules = [{
      packages.$pkgA.components.library.doCoverage = true;
      packages.$pkgB.components.library.doCoverage = true;
    }];
  };

  # Generate a coverage report for $pkgA that only includes the
  # unit-test check and only shows coverage information for $pkgA, not
  # $pkgB.
  custom$pkgACoverageReport = haskellLib.coverageReport rec {
    name = "$pkgA-unit-tests-only"
    inherit (project.$pkgA.components) library;
    checks = [project.$pkgA.components.checks.unit-test];
    # Note that this is the default value of the "mixLibraries"
    # argument and so this line isn't really necessary.
    mixLibraries = [project.$pkgA.components.library];
  };

  custom$pkgBCoverageReport = haskellLib.coverageReport rec {
    name = "$pkgB-unit-tests-only"
    inherit (project.$pkgB.components) library;
    checks = [project.$pkgB.components.checks.unit-test];
    mixLibraries = [project.$pkgB.components.library];
  };
 
  # Generate a project coverage report that only includes the unit
  # tests of the project, and only shows how each unit test effects
  # the coverage of it's package, and not other packages in the
  # project.
  allUnitTestsProjectReport = haskellLib.projectCoverageReport [custom$pkgACoverageReport custom$pkgBCoverageReport];
in {
  inherit project custom$pkgACoverageReport custom$pkgBCoverageReport allUnitTestsProjectCoverageReport;
}