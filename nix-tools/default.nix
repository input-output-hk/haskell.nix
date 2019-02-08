{ symlinkJoin, fetchExternal, mkPkgSet }:

let
  src = fetchExternal {
    name     = "nix-tools-src";
    specJSON = ./nix-tools-src.json;
    override = "nix-tools";
  };

  hsPkgs = import (src + "/pkgs.nix") { inherit mkPkgSet; };
in
  symlinkJoin {
    name = "nix-tools";
    paths = builtins.attrValues hsPkgs.nix-tools.components.exes;
  }
