# This default.nix is designed to be passed directly nixpkgs with something like:
#   import <nixpkgs> (builtins.fetchTarball https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz)
{ config   = import ./config.nix;
  overlays = import ./overlays;
}