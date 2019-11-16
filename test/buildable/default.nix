{ stdenv, mkCabalProjectPkgSet, callCabalProjectToNix, importAndFilterProject }:

with stdenv.lib;

let
  plan = importAndFilterProject (callCabalProjectToNix {
    name = "test-buildable";
    index-state = "2019-04-30T00:00:00Z";
    src = ./.;
  });
  pkgSet = mkCabalProjectPkgSet {
    plan-pkgs = plan.pkgs;
    modules = [ { packages.buildable-test.flags.exclude-broken = true; } ];
  };
  packages = pkgSet.config.hsPkgs;
in
  stdenv.mkDerivation {
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
      inherit pkgSet packages;
      inherit (plan) nix;
    };
  }
