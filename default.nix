rec {
  sources = {
    inherit (import ./nixpkgs/default.nix) nixpkgs-1903 nixpkgs-1909 nixpkgs-default;
  };

  config   = import ./config.nix;
  overlays = import ./overlays;
  nixpkgsArgs = { inherit overlays config; };
}
