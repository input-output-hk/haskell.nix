rec {
  # nixpkgsArgs is designed to be passed directly nixpkgs with something like:
  #   import <nixpkgs> (import (builtins.fetchTarball https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz)).nixpkgsArgs
  nixpkgsArgs = {
    config   = import ./config.nix;
    overlays = import ./overlays;
  };

  # nixpkgs allows you to import haskell.nix with the default pinned nixpkgs
  #   (import (builtins.fetchTarball https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz)).nixpkgs {}
  defaultNixpkgs = nixpkgs-pins."19.09";
  
  # nixpkgs-pins allows you to import haskell.nix with one of the pinned nixpkgs
  #   (import (builtins.fetchTarball https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz)).nixpkgs-pins."release-19.03" {}
  nixpkgs-pins = builtins.mapAttrs
    (_: nixpkgs-name: import ./nixpkgs { nixpkgs-pin = nixpkgs-name; }) {
      "18.09" = "release-18.09";
      "19.03" = "release-19.03";
      "19.09" = "release-19.09";
    };  
}
