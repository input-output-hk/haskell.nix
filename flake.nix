{
  description = "Alternative Haskell Infrastructure for Nixpkgs";

  outputs = { self }: let
    config = import ./config.nix;
    # We can't import ./nix/sources.nix directly, because that uses nixpkgs to fetch by default,
    # and importing nixpkgs without specifying localSystem doesn't work on flakes.
    sources = let
      sourcesInfo =
        builtins.fromJSON (builtins.readFile ./nix/sources.json);
      fetch = sourceInfo:
        builtins.fetchTarball { inherit (sourceInfo) url sha256; };
    in builtins.mapAttrs (_: fetch) sourcesInfo;

    nixpkgsArgs = {
      inherit config;
      overlays = [ self.overlay ];
    };
  in {
    # Using the eval-on-build version here as the plan is that
    # `builtins.currentSystem` will not be supported in flakes.
    # https://github.com/NixOS/rfcs/pull/49/files#diff-a5a138ca225433534de8d260f225fe31R429
    overlay = self.overlays.combined-eval-on-build;
    overlays = import ./overlays { sourcesOverride = sources; };
    legacyPackages = let
      genAttrs = lst: f:
        builtins.listToAttrs (map (name: {
          inherit name;
          value = f name;
        }) lst);
    in genAttrs [ "x86_64-linux" "x86_64-darwin" ] (system:
      import sources.nixpkgs
      (nixpkgsArgs // { localSystem = { inherit system; }; }));
  };
}
