# This file contains the package set used by the release.nix jobset.
#
# It is separate from default.nix because that file is the public API
# of Haskell.nix, which shouldn't have tests, etc.

{ nixpkgs ? ./nixpkgs
# Provide args to the nixpkgs instantiation.
, system ? builtins.currentSystem
, crossSystem ? null
, config ? {}
, nixpkgsArgs ? { inherit system crossSystem; }
}:

let
  haskellNixArgs = import ./default.nix;
  pkgs = import nixpkgs ({
    config   = haskellNixArgs.config // config;
    overlays = haskellNixArgs.overlays ++
      [(self: super: {
        darcs = (self.haskell-nix.hackage-package {
          name = "darcs";
          version = "2.14.2";
          # Apply the latest darcs.net Setup.hs patches
          modules = [{packages.darcs.patches = [ ./patches/darcs-setup.patch ];}];
        }).components.exes.darcs;
      })]; } // nixpkgsArgs);
  haskell = pkgs.haskell-nix;

in {
  inherit (haskell) nix-tools source-pins;
  tests = import ./test/default.nix { inherit nixpkgs nixpkgsArgs; };

  # Scripts for keeping Hackage and Stackage up to date, and CI tasks.
  maintainer-scripts = pkgs.dontRecurseIntoAttrs {
    update-hackage = haskell.callPackage ./scripts/update-hackage.nix {};
    update-stackage = haskell.callPackage ./scripts/update-stackage.nix {};
    update-pins = haskell.callPackage ./scripts/update-pins.nix {};
    update-docs = haskell.callPackage ./scripts/update-docs.nix {
      generatedOptions = import ./scripts/options-doc.nix {
        # nixpkgs unstable changes "Option has no description" from an
        # error into a warning. That is quite helpful when hardly any
        # of our options are documented, thanks @oxij.
        pkgs = import (pkgs.fetchFromGitHub {
          owner = "NixOS";
          repo = "nixpkgs";
          rev = "4ab1c14714fc97a27655f3a6877386da3cb237bc";
          sha256 = "16lcj9552q2jfxc27j6116qkf2vl2dcj7vhg5gdq4qi51d891yhn";
        }) {};
      };
    };
    check-hydra = haskell.callPackage ./scripts/check-hydra.nix {};
    check-closure-size = haskell.callPackage ./scripts/check-closure-size.nix {};
  };
}
