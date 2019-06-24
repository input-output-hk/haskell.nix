{ pkgs ? import <nixpkgs> {}
, lib ? pkgs.lib
}:
let
  # Provide a dummy pkg-def for the package set function
  pkg-def = hackage: {
    packages = {};
    compiler = {
      version = "";
      nix-name = "";
      packages = {};
    };
  };

  # Apply the package set function to get NixOS options.
  inherit (import ../package-set.nix { hackage = null; inherit pkgs pkg-def; }) options;

  # Use code from the NixOS manual to generate Docbook XML options list.
  manual = import (pkgs.path + /nixos/doc/manual) {
    inherit pkgs options;
    config.meta.doc = [];
    version = "testing";
    revision = "testing";
  };
in
  # Convert docbook options list to markdown
  pkgs.runCommand "options.md" {} ''
    ${pkgs.buildPackages.pandoc}/bin/pandoc -s -f docbook -t markdown_strict ${manual.generatedSources}/options-db.xml -o $out
  ''
