{ pkgs ? import <nixpkgs> {} }:
let
  fetch = jsonFile:
    with builtins;
    let spec = fromJSON (readFile jsonFile);
    in pkgs.fetchgit {
      name = "old-ghc-nix";
      inherit (spec) sha256 url rev;
    };
in import (fetch ./old-ghc-nix.json) { inherit pkgs; }
