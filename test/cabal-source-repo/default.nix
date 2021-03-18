{ stdenv, lib, cabalProject', recurseIntoAttrs, haskellLib, testSrc, compiler-nix-name }:

with lib;

let
  project = cabalProject' {
    inherit compiler-nix-name;
    index-state = "2020-05-25T00:00:00Z";
    src = testSrc "cabal-source-repo";
  };
  packages = project.hsPkgs;
in recurseIntoAttrs {
  ifdInputs = {
    inherit (project) plan-nix;
  };
  run = stdenv.mkDerivation {
    name = "call-cabal-project-to-nix-test";

    buildCommand = ''
      exe="${packages.use-cabal-simple.components.exes.use-cabal-simple.exePath}"

      printf "checking whether executable runs... " >& 2
      cat ${haskellLib.check packages.use-cabal-simple.components.exes.use-cabal-simple}/test-stdout

      touch $out
    '';

    meta.platforms = platforms.all;

    passthru = {
      # Attributes used for debugging with nix repl
      inherit packages;
    };
  };
}
