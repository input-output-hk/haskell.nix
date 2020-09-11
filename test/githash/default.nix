{ stdenv, haskell-nix, recurseIntoAttrs, testSrc, compiler-nix-name }:

with stdenv.lib;

let
  project = haskell-nix.cabalProject' {
    src = testSrc "githash";
    inherit compiler-nix-name;
  };

  packages = project.hsPkgs;
  githash-test =
    packages.githash-test.components.exes.githash-test;

in recurseIntoAttrs {
  ifdInputs = {
    inherit (project) plan-nix;
  };

  run = stdenv.mkDerivation {
    name = "run-githash-test";

    buildCommand = ''
      exe="${githash-test}/bin/githash-test${stdenv.hostPlatform.extensions.executable}"
      echo Checking that the error message is generated and that it came from the right place:
      (${toString githash-test.config.testWrapper} $exe || true) 2>&1 \
        | grep "error, called at src/Main.hs:5:13 in main:Main"
      touch $out
    '';

    meta.platforms = platforms.all;

    passthru = {
      # Used for debugging with nix repl
      inherit project packages;
    };
  };
}
