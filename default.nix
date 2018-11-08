{ pkgs ? import <nixpkgs> {} }:
let
  hsPkgs = import ./pkgs.nix { inherit pkgs; };
in
    hsPkgs // {
      nix-tools-all-execs = pkgs.symlinkJoin
        { name = "nix-tools";
          paths = builtins.attrValues hsPkgs.nix-tools.components.exes; };
    }
