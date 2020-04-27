{
  description = "Alternative Haskell Infrastructure for Nixpkgs";

  edition = 201909;

  outputs = { self }: {
    overlay = final: prev:
      let
        composeExtensions = f: g: self: super:
          let
            fApplied = f self super;
            super' = super // fApplied;
          in fApplied // g self super';
      in (builtins.foldl' composeExtensions (_: _: { }) (import ./overlays))
      final prev;
    config = import ./config.nix;
    sources = import ./nixpkgs;
    checks = let
      nixpkgs = system:
        import self.sources.nixpkgs-default {
          localSystem = { inherit system; };
        };
      systems = [ "x86_64-linux" "x86_64-darwin" ];
      forAllSystems = f:
        builtins.listToAttrs (map (system: {
          name = system;
          value = builtins.listToAttrs (map (test: {
            name = (builtins.parseDrvName test.name).name;
            value = test;
          }) (f system));
        }) systems);
      removeShells =
        builtins.filter (x: isNull (builtins.match ".*shell-for.*" x.name));
      tests = system:
        import ./test {
          nixpkgsArgs = {
            inherit (self) config;
            overlays = [ self.overlay ];
            localSystem = { inherit system; };
          };
        };
      testDrvs = system:
        with nixpkgs system;
        with lib;
        removeShells (collect isDerivation (tests system));
    in forAllSystems testDrvs;
  };
}
