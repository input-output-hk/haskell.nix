{
  lib
, haskellLib
}: {
  # Provide a devshell profile (https://github.com/numtide/devshell),
  # adapted from the project normal shell.
  devshell = final: prev: {
    devshell = let
    in {
      packages = final.shell.nativeBuildInputs
      # devshell does not use pkgs.mkShell / pkgs.stdenv.mkDerivation,
      # so we need to explicit required dependencies which
      # are provided implicitely by stdenv when using the normal shell:
      ++ final.shell.stdenv.defaultNativeBuildInputs
      ++ final.shell.stdenv.extraNativeBuildInputs;
      env = lib.mapAttrsToList lib.nameValuePair {
        inherit (final.shell) CABAL_CONFIG NIX_GHC_LIBDIR;
      };
    };
  };

  # Provides easily accessible attrset for each type of
  # components belonging to the project packages.
  projectComponents = final: prev: {
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
