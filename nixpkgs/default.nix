# This file is for backwards compatibility only
with import ../nix/sources.nix {};
{
  inherit nixpkgs-2003 nixpkgs-1909 nixpgks;
  nixpkgs-default = nixpkgs;
}
