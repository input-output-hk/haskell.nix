{ stdenv, mkCabalProjectPkgSet, callCabalProjectToNix, importAndFilterProject }:

with stdenv.lib;

let
  plan = importAndFilterProject (callCabalProjectToNix {
    index-state = "2019-04-30T00:00:00Z";
    src = ./.;
  });
  pkgSet = mkCabalProjectPkgSet {
    plan-pkgs = plan.pkgs;
    # We need mkOverride to override the value from `plan.json`.
    # Normally we could set the flag in `cabal.project`,
    # but for this test that does not work because then cabal would
    # see the exe is not buildable and exclude it before haskell.nix
    # can.
    modules = [ { packages.buildable-test.flags.exclude-broken = mkOverride 10 true; } ];
  };
  packages = pkgSet.config.hsPkgs;
in
  stdenv.mkDerivation {
    name = "buildable-test";

    buildCommand = 
      (concatStrings (mapAttrsToList (name: value: ''
        exe="${value}/bin/${name}"

        printf "checking whether executable runs... " >& 2
        $exe
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
