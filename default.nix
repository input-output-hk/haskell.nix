{ pkgs ? import <nixpkgs> {} }:
let
  hsPkgs = import ./pkgs.nix { inherit pkgs; };
in
  if ! builtins.pathExists ./hackage-db/hackage-db.cabal
  then abort ''

    The `hackage-db' submodule is not initialized.
    Run `git submodule update --init`.
  ''
  else
    hsPkgs // {
      nix-tools-all-execs = pkgs.symlinkJoin
        { name = "nix-tools";
          paths = builtins.attrValues hsPkgs.nix-tools.components.exes; };
    }
