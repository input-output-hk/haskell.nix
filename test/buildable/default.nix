{ stdenv, cabalProject', haskellLib, recurseIntoAttrs, testSrc, compiler-nix-name }:

with stdenv.lib;

let
  project = cabalProject' {
    inherit compiler-nix-name;
    index-state = "2020-05-25T00:00:00Z";
    src = testSrc "buildable";
    modules = [ { packages.buildable-test.flags.exclude-broken = true; } ];
  };
  packages = project.hsPkgs;

in recurseIntoAttrs {
  ifdInputs = {
    inherit (project) plan-nix;
  };
  run = stdenv.mkDerivation {
    name = "buildable-test";

    buildCommand =
      (concatStrings (mapAttrsToList (name: value: ''
        printf "checking whether executable runs... " >& 2
        cat ${haskellLib.check value}/test-stdout
      '') packages.buildable-test.components.exes)) + ''
      touch $out
    '';

    meta.platforms = platforms.all;

    passthru = {
      # Attributes used for debugging with nix repl
      inherit project packages;
    };
  };
}
