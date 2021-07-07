{
  description = "Alternative Haskell Infrastructure for Nixpkgs";

  inputs = {
    # Note: keep this in sync with sources.json!
    nixpkgs.url = github:NixOS/nixpkgs/3c6f3f84af60a8ed5b8a79cf3026b7630fcdefb8;
    nixpkgs-2009.url = github:NixOS/nixpkgs/46d1c3f28ca991601a53e9a14fdd53fcd3dd8416;
    nixpkgs-2105.url = github:NixOS/nixpkgs/3c6f3f84af60a8ed5b8a79cf3026b7630fcdefb8;
    nixpkgs-unstable.url = github:NixOS/nixpkgs/0747387223edf1aa5beaedf48983471315d95e16;
  };

  outputs = { self, nixpkgs, ... }:
  {

    internal = rec {
      config = import ./config.nix;
      # Use a shim for pkgs that does not depend on `builtins.currentSystem`.
      sources = import ./nix/sources.nix {
        pkgs = { fetchzip = builtins.fetchTarball; };
      };

      nixpkgsArgs = {
        inherit config;
        overlays = [ self.overlay ];
      };

      overlaysOverrideable = import ./overlays;
    };

    overlay = self.overlays.combined;
    overlays = self.internal.overlaysOverrideable { sourcesOverride = self.internal.sources; };

    legacyPackages = let
      genAttrs = lst: f:
        builtins.listToAttrs (map (name: {
          inherit name;
          value = f name;
        }) lst);
    in genAttrs [ "x86_64-linux" "x86_64-darwin" ] (system:
      import nixpkgs
      (self.internal.nixpkgsArgs // { localSystem = { inherit system; }; }));
  };
}
