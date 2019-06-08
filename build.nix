# This file contains the package set used by the release.nix jobset.
#
# It is separate from default.nix because that file is the public API
# of Haskell.nix, which shouldn't have tests, etc.

{ pkgs ? import nixpkgs nixpkgsArgs
, nixpkgs ? ./nixpkgs
# Provide args to the nixpkgs instantiation.
, system ? builtins.currentSystem
, crossSystem ? null
, config ? {}
, nixpkgsArgs ? { inherit system crossSystem config; }
}:

let
  haskell = import ./default.nix { inherit pkgs; };

in {
  inherit (haskell) nix-tools source-pins;
  tests = import ./test/default.nix { inherit haskell; };

  # Scripts for keeping Hackage and Stackage up to date, and CI tasks.
  maintainer-scripts = pkgs.dontRecurseIntoAttrs {
    update-hackage = haskell.callPackage ./scripts/update-hackage.nix {};
    update-stackage = haskell.callPackage ./scripts/update-stackage.nix {};
    update-pins = haskell.callPackage ./scripts/update-pins.nix {};
    update-docs = haskell.callPackage ./scripts/update-docs.nix {};
    check-hydra = haskell.callPackage ./scripts/check-hydra.nix {};
  };
}
