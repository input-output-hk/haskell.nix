{ stdenv, fetchFromGitHub, recurseIntoAttrs, runCommand, testSrc, compiler-nix-name, git, hpc-coveralls, jq }:

with stdenv.lib;

let
  exampleProjectSrc = fetchFromGitHub {
    owner  = "input-output-hk";
    repo   = "cardano-shell";
    rev    = "8e0d6f5548c1c13a9edfeb56e4ea27c0a6d10948";
    sha256 = "07yjl3agiacmy5irxxmrmm7ffpnvz84zgrylnp3wmcrn417pc5fq";
  };

  exampleProject = import "${exampleProjectSrc}" { config = { haskellNix = { coverage = true; }; }; };
  exampleCoverageReport = exampleProject.cardanoShellHaskellPackages.projectCoverageReport;

in recurseIntoAttrs ({
  run = stdenv.mkDerivation {
    name = "coverage-golden-test";

    buildInputs = [ git jq ];
    buildCommand = ''
      ########################################################################
      # Test that the coverage reports haven't materially changed

      TRAVIS=1 TRAVIS_JOB_ID=1 ${hpc-coveralls}/bin/hpc-coveralls all \
        --package-dir ${exampleProjectSrc}/cardano-shell/ \
        --package-dir ${exampleProjectSrc}/cardano-launcher \
        --hpc-dir ${exampleCoverageReport}/share/hpc/vanilla \
        --coverage-mode StrictlyFullLines \
        --dont-send

      # Format JSON for better diffing error messages and remove
      # references to Nix path, our golden coverage file contains no
      # Nix paths
      cat ./travis-ci-1.json | jq -S | sed "s;${exampleProjectSrc}/;;g" > ./actual.json

      # Fail if doesn't match golden, i.e. our coverage has changed
      diff ${./golden.json} ./actual.json

      touch $out
    '';

    meta.platforms = platforms.all;
  };
})
