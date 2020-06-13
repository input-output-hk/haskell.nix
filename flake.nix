{
  description = "Alternative Haskell Infrastructure for Nixpkgs";

  edition = 201909;

  outputs = { self }: {
    # Using the eval-on-build version here as the plan is that
    # `builtins.currentSystem` will not be supported in flakes.
    # https://github.com/NixOS/rfcs/pull/49/files#diff-a5a138ca225433534de8d260f225fe31R429
    overlay = (self.overlays {}).combined-eval-on-build;
    overlays = import ./overlays;
    config = import ./config.nix;
    sources = import ./nixpkgs;
    nixpkgsArgs = {
      inherit (self) config;
      overlays = [ self.overlay ];
    };
    legacyPackages = let
      genAttrs = lst: f:
        builtins.listToAttrs (map (name: {
          inherit name;
          value = f name;
        }) lst);
    in genAttrs [ "x86_64-linux" "x86_64-darwin" ] (system:
      import self.sources.nixpkgs-default
      (self.nixpkgsArgs // { localSystem = { inherit system; }; }));
  };
}
