{ pkgs ? import <nixpkgs> {} }:
let
 base = import ./pkgs.nix { inherit pkgs; };
in
 base // { nix-tools-all-execs = pkgs.symlinkJoin
           { name = "nix-tools";
             paths = builtins.attrValues base.hsPkgs.nix-tools.components.exes; }; }
