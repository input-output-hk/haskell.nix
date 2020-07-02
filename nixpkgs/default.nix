# This file is for backwards compatibility only
rec {
  inherit (import ../nix/sources.nix) nixpkgs-2003 nixpkgs-1909 nixpkgs;
  nixpkgs-default = nixpkgs;
}
