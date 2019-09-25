{ nixpkgs ? builtins.fetchTarball https://github.com/NixOS/nixpkgs/archive/2255f292063ccbe184ff8f9b35ce475c04d5ae69.tar.gz }:

let
  pkgs = import (builtins.fetchTarball https://github.com/input-output-hk/haskell.nix/archive/hkm/overlays-2.tar.gz) { inherit nixpkgs; };

  pkgSet = pkgs.haskell-nix.mkCabalProjectPkgSet {
    plan-pkgs = import ./pkgs.nix;
    pkg-def-extras = [];
    modules = [];
  };

in
  pkgSet.config.hsPkgs
