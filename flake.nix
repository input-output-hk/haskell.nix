{
  description = "Alternative Haskell Infrastructure for Nixpkgs";

  edition = 201909;

  outputs = { self }: {
    overlay = self.overlays.combined;
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
