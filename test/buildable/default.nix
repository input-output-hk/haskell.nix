{ stdenv, cabalProject', haskellLib, recurseIntoAttrs }:

with stdenv.lib;

let
  project = cabalProject' {
    name = "test-buildable";
    index-state = "2019-04-30T00:00:00Z";
    src = haskellLib.cleanGit { src = ../..; subDir = "test/buildable"; };
    modules = [ { packages.buildable-test.flags.exclude-broken = true; } ];
  };
  packages = project.hsPkgs;

in recurseIntoAttrs {
  inherit (project) plan-nix;
  run = stdenv.mkDerivation {
    name = "buildable-test";

    buildCommand = 
      (concatStrings (mapAttrsToList (name: value: ''
        printf "checking whether executable runs... " >& 2
        cat ${value.run}
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