{ pkgs ? import <nixpkgs> {} }:

{ overrides ? (_: _: {})
, root
, shell ? false
, stackage ? fetchGit { url = "https://github.com/typeable/nixpkgs-stackage"; rev = "6042df5e646d65b826add0a85d16304bee8e1dd5"; } }:

let
  lib = import ./lib.nix pkgs;

  buildProject = import ./build.nix {
    pkgs = pkgs // { lib = pkgs.lib // lib; };
    inherit overrides shell stackage;
  };
in

buildProject (lib.importYAML "${root}/stack.yaml") root
