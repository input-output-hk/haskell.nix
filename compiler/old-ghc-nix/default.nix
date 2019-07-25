{ pkgs ? import <nixpkgs> {} }:
let
  fetch = jsonFile:
    with builtins;
    let spec = fromJSON (readFile jsonFile);
    in fetchTarball {
      name = "old-ghc-nix";
      inherit (spec) sha256;
      url = "${spec.url}/archive/${spec.rev}.tar.gz";
    };
in import (fetch ./old-ghc-nix.json) { inherit pkgs; }
