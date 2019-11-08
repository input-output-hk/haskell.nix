let
  # Names nixpkgs pins (corrisponds to names of the nixpkgs/*.json files)
  nixpkgs-names = ["release-18.08" "release-19.03" "release-19.09"];

  # Name of the default nixpkgs-pin
  default-nixpkgs-name = "release-19.09";

  # These are from lib.attrset but we don't want to have to import a nixpkgs just to get them
  nameValuePair = name: value: { inherit name value; };
  genAttrs = names: f: builtins.listToAttrs (builtins.map (n: nameValuePair n (f n)) names);

in {
  # nixpkgsArgs is designed to be passed directly nixpkgs with something like:
  #   import <nixpkgs> (import (builtins.fetchTarball https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz)).nixpkgsArgs
  nixpkgsArgs = {
    config   = import ./config.nix;
    overlays = import ./overlays;
  };

  inherit default-nixpkgs-name;
  
  # nixpkgs allows you to import haskell.nix with the default pinned nixpkgs
  #   (import (builtins.fetchTarball https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz)).nixpkgs {}
  nixpkgs = args: import ./nixpkgs (args // { nixpkgs-pin = default-nixpkgs-name; });
  
  # nixpkgs-pins allows you to import haskell.nix with one of the pinned nixpkgs
  #   (import (builtins.fetchTarball https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz)).nixpkgs-pins."release-19.03" {}
  nixpkgs-pins = (genAttrs nixpkgs-names
      (nixpkgs-name: args: import ./nixpkgs (args // { nixpkgs-pin = nixpkgs-name; })));
}
