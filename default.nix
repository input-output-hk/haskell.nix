{ pkgs ? import nixpkgs ({ config = config; overlays = overlays; } // nixpkgsArgs)
# Use a pinned nixpkgs rather than the one on NIX_PATH
, nixpkgs ? <nixpkgs> #./nixpkgs
# default config we provide to nixpkgs.
, config ? import ./config.nix
# default overlays
, overlays ? import ./overlays
# Provide additional args to the nixpkgs instantiation. This can also override
# config and overlays.
, nixpkgsArgs ? { }
}:
pkgs
