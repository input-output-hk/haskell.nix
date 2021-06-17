{
  description = "Alternative Haskell Infrastructure for Nixpkgs";

  inputs = {
    # Note: keep this in sync with sources.json!
    nixpkgs.url = github:NixOS/nixpkgs/f02bf8ffb9a5ec5e8f6f66f1e5544fd2aa1a0693;
    nixpkgs-2009.url = github:NixOS/nixpkgs/f02bf8ffb9a5ec5e8f6f66f1e5544fd2aa1a0693;
    nixpkgs-unstable.url = github:NixOS/nixpkgs/d8eb97e3801bde96491535f40483d550b57605b9;
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

    # Using the eval-on-build version here as the plan is that
    # `builtins.currentSystem` will not be supported in flakes.
    # https://github.com/NixOS/rfcs/pull/49/files#diff-a5a138ca225433534de8d260f225fe31R429
    overlay = self.overlays.combined-eval-on-build;
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
