{ symlinkJoin, mkCabalProjectPkgSet }:

let
  pkgSet = mkCabalProjectPkgSet {
    plan-pkgs = import ./pkgs.nix;
    pkg-def-extras = [];
    modules = [];
  };

  hsPkgs = pkgSet.config.hsPkgs;

in
  symlinkJoin {
    name = "nix-tools";
    paths = builtins.attrValues hsPkgs.nix-tools.components.exes;
  }
