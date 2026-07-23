# Test the per-component `buildJobs` option (#1479).
#
# `buildJobs` controls the `-jN` flag passed to `Setup build` and the
# derivation's `enableParallelBuilding`.  We assert on the component
# derivation's `buildPhase` / `enableParallelBuilding` attributes directly
# (an eval-level check) so the test does not need to compile anything.
{ stdenv, lib, project', haskellLib, testSrc, compiler-nix-name, evalPackages, runCommand }:

with lib;

let
  mkProject = extra: project' {
    inherit compiler-nix-name evalPackages;
    src = testSrc "cabal-simple";
    modules = [
      ({
        # cabal-simple's library has no exposed modules -> haddock fails.
        packages.cabal-simple.doHaddock = false;
      } // extra)
    ];
  };

  # Default project: no `buildJobs` set.
  defaultExe = (mkProject {}).hsPkgs.cabal-simple.components.exes.cabal-simple;

  # `buildJobs = 1`: sequential build.
  seqExe = (mkProject {
    packages.cabal-simple.components.exes.cabal-simple.buildJobs = 1;
  }).hsPkgs.cabal-simple.components.exes.cabal-simple;

  # `buildJobs = 3`: pinned parallelism.
  pinnedExe = (mkProject {
    packages.cabal-simple.components.exes.cabal-simple.buildJobs = 3;
  }).hsPkgs.cabal-simple.components.exes.cabal-simple;

  cappedDefault = "-j$(($NIX_BUILD_CORES > 4 ? 4 : $NIX_BUILD_CORES))";

  checks = [
    # Default is unchanged: capped `-j` and parallel building on.
    { name = "default keeps capped -j";
      ok = hasInfix cappedDefault defaultExe.buildPhase; }
    { name = "default enables parallel building";
      ok = defaultExe.enableParallelBuilding == true; }
    # buildJobs = 1 -> `-j1`, parallel building off, capped default gone.
    { name = "buildJobs=1 passes -j1";
      ok = hasInfix "-j1 " seqExe.buildPhase; }
    { name = "buildJobs=1 disables parallel building";
      ok = seqExe.enableParallelBuilding == false; }
    { name = "buildJobs=1 drops the capped default";
      ok = !(hasInfix cappedDefault seqExe.buildPhase); }
    # buildJobs = 3 -> `-j3`, parallel building on.
    { name = "buildJobs=3 passes -j3";
      ok = hasInfix "-j3 " pinnedExe.buildPhase; }
    { name = "buildJobs=3 keeps parallel building";
      ok = pinnedExe.enableParallelBuilding == true; }
  ];

  failures = filter (c: !c.ok) checks;

in lib.recurseIntoAttrs {
  ifdInputs = {
    inherit ((mkProject {})) plan-nix;
  };

  run = assert lib.assertMsg (failures == [])
    "build-jobs test failed: ${toString (map (c: c.name) failures)}";
    runCommand "build-jobs-test" { passthru = { inherit defaultExe seqExe pinnedExe; }; } ''
      printf "build-jobs: all %d eval checks passed\n" ${toString (length checks)} >&2
      touch $out
    '';
}
