# This default.nix is designed to be passed directly nixpkgs with something like:
#   import <nixpkgs> (import (builtins.fetchTarball https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz))
{ config   = import ./config.nix;
  overlays = import ./overlays;
  # convenience function. To make it possible to call
  #   import <nixpkgs> (import (...).withArgs { system = builtins.currentSystem; crossSystem = null; config = { ... }; overlays = [...]; }
  # instead of having this convoluted args reshuffling in the client code.
  withArgs = args: args // { config = config // args.config; overlays = overlays ++ args.overlays; };
}
