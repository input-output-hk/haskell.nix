# Coverage

haskell.nix can generate coverage information for your package or
project using Cabal's inbuilt hpc support.

## Prerequisites

To get a sensible coverage report, you need to enable coverage on each
of the packages of your project:

```nix
pkgs.haskell-nix.project {
  src = pkgs.haskell-nix.haskellLib.cleanGit {
    name = "haskell-nix-project";
    src = ./.;
  };
  compiler-nix-name = "ghc884";

  modules = [{
    packages.$pkg.components.library.doCoverage = true;
  }];
}
```

If you would like to make coverage optional, add an argument to your nix expression:

```nix
{ withCoverage ? false }:

pkgs.haskell-nix.project {
  src = pkgs.haskell-nix.haskellLib.cleanGit {
    name = "haskell-nix-project";
    src = ./.;
  };
  compiler-nix-name = "ghc884";

  modules = pkgs.lib.optional withCoverage [{
    packages.$pkg.components.library.doCoverage = true;
  }];
}
```

## Per-package

```bash
nix-build default.nix -A "projectWithCoverage.$pkg.coverageReport"
```

This will generate a coverage report for the package you requested.
All tests that are enabled (configured with `doCheck == true`) are
included in the coverage report.

See the [developer coverage docs](../dev/coverage.md#package-reports) for more information.

## Project-wide

```bash
nix-build default.nix -A "projectWithCoverage.projectCoverageReport"
```

This will generate a coverage report for all the local packages in
your project.

See the [developer coverage docs](../dev/coverage.md#project-wide-reports) for more information.

## Custom

By default, the behaviour of the `coverageReport` attribute is to
generate a coverage report that describes how that package affects the
coverage of all local packages (including itself) in the project.

The default behaviour of `projectCoverageReport` is to sum the
default coverage reports (produced by the above process) of all local
packages in the project.

You can modify this behaviour by using the `coverageReport` and
`projectCoverageReport` functions found in the haskell.nix library:

```nix
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
```
