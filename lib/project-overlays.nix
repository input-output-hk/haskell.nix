{
  lib
, haskellLib
}: {

  # TODO: remove by end of 2022.
  devshell = final: _prev: {
    devshell = builtins.trace "WARNING: `projectOverlays.devshell` is deprecated in favor of `haskellLib.devshellFor`"
      (haskellLib.devshellFor final.shell);
  };

  # Provides easily accessible attrset for each type of
  # components belonging to the project packages.
  projectComponents = final: _prev: {
    # local project packages:
    packages = haskellLib.selectProjectPackages final.hsPkgs;
    # set of all exes (as first level entries):
    exes = lib.foldl' lib.mergeAttrs { } (map (p: p.components.exes) (lib.attrValues final.packages));
    # `tests` are the test suites which have been built.
    tests = haskellLib.collectComponents' "tests" final.packages;
    # `benchmarks` (only built, not run).
    benchmarks = haskellLib.collectComponents' "benchmarks" final.packages;
    # `checks` collect results of executing the tests:
    checks = haskellLib.collectChecks' final.packages;
  };

}
