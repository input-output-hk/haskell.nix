{ pkgs
, haskell
, stdenv
}:

with stdenv.lib;

let
  pkgSet = haskell.mkPkgSet {
    inherit pkgs;
    # generated with:
    #   cabal new-build
    #   plan-to-nix dist-newstyle/cache/plan.json > plan.nix 
    #   cabal-to-nix test-with-packages.cabal > test-with-packages.nix 
    pkg-def = import ./plan.nix;
    pkg-def-overlays = [
      { test-with-packages = ./test-with-packages.nix; }
    ];
    modules = [
      # overrides to fix the build
      {
        packages.transformers-compat.components.library.doExactConfig = true;
      }

      # vary component config for tests
      {
        packages.test-with-packages.components = {
          all.doExactConfig = mkForce false; # the default; use force because of quirk in all. See package.nix
          library.doExactConfig = true; # not the default
        };
      }
    ];
  };

  packages = pkgSet.config.hsPkgs;

  # Add cabal as a buildInput for a haskell derivation. Useful for nix-shell.
  addCabalInstall = drv: drv.overrideAttrs (oldAttrs: {
    buildInputs = (oldAttrs.buildInputs or []) ++ [ pkgs.cabal-install ];
  });

in
  stdenv.mkDerivation {
    name = "with-packages-test";

    buildCommand = let
      package = packages.test-with-packages;
      inherit (package.components) library;
    in ''
      ########################################################################
      # test with-packages

      printf "checking that the 'all' component works... " >& 2
      echo ${package.components.all} >& 2

      printf "checking that the 'library' component works... " >& 2
      echo ${package.components.library} >& 2

      printf "checking that the package env has the dependencies... " >& 2
      ${package.components.all.env}/bin/runghc ${./Point.hs}
      echo >& 2

      printf "checking that components.library.env has the dependencies... " >& 2
      ${library.env}/bin/runghc ${./Point.hs}
      # echo >& 2

      touch $out
    '';

    meta.platforms = platforms.all;
    passthru = {
      # Used for debugging with nix repl
      inherit packages pkgSet;

      # Used for testing externally with nix-shell (../tests.sh).
      # This just adds cabal-install to the existing shells.
      test-shell = addCabalInstall packages.test-with-packages.components.all;

      # A variant of test-shell with the component option doExactConfig enabled
      test-shell-dec = addCabalInstall packages.test-with-packages.components.library;
    };
}
