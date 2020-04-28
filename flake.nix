{
  description = "Alternative Haskell Infrastructure for Nixpkgs";

  edition = 201909;

  outputs = { self }: {
    # Nix is really particular about argument names here;
    # It requires that `overlay` and each of `overlays` have `final` and `prev` as arguments.
    overlay = final: prev:
    let
      composeExtensions = f: g: self: super:
      let
        fApplied = f self super;
        super' = super // fApplied;
      in fApplied // g self super';
    in (builtins.foldl' composeExtensions (_: _: { }) (import ./overlays))
    final prev;
    overlays =  {
      release1903 = final: prev: import ./overlays/release-19.03.nix final prev;
      wine = final: prev: import ./overlays/wine.nix final prev;
      haskell = final: prev: import ./overlays/haskell.nix final prev;
      bootstrap = final: prev: import ./overlays/bootstrap.nix final prev;
      ghc = final: prev: import ./overlays/ghc.nix final prev;
      ghc-packages = final: prev: import ./overlays/ghc-packages.nix final prev;
      windows = final: prev: import ./overlays/windows.nix final prev;
      armv6l-linux = final: prev: import ./overlays/armv6l-linux.nix final prev;
      musl = final: prev: import ./overlays/musl.nix final prev;
    };

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
