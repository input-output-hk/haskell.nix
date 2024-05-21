{

  inputs = {
    haskell-nix = {
      url = "github:input-output-hk/haskell.nix";
      inputs.hackage.follows = "hackage";
    };

    nixpkgs.follows = "haskell-nix/nixpkgs";

    hackage = {
      url = "github:input-output-hk/hackage.nix";
      flake = false;
    };

    CHaP = {
      url = "github:IntersectMBO/cardano-haskell-packages?ref=repo";
      flake = false;
    };

    iohk-nix = {
      url = "github:input-output-hk/iohk-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, haskell-nix, iohk-nix, ... }:
    let
      mkNixpkgs = system: import nixpkgs {
        inherit system;
        config = haskell-nix.config;
        overlays = [
          iohk-nix.overlays.crypto
          iohk-nix.overlays.cardano-lib
          haskell-nix.overlay
          iohk-nix.overlays.haskell-nix-crypto
          iohk-nix.overlays.haskell-nix-extra
        ];
      };

      forAllSystems = f:
        nixpkgs.lib.genAttrs 
          [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ]
          (system: f (mkNixpkgs system));
    in
      {
        devShells = forAllSystems (pkgs: {
          default = (pkgs.haskell-nix.cabalProject' { 
            src = ./.;
            name = "test";
            compiler-nix-name = pkgs.lib.mkDefault "ghc965";
            shell.withHoogle = false;
          }).shell;
        });
      };

  nixConfig = {
    extra-substituters = [
      "https://cache.iog.io"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];
    allow-import-from-derivation = true;
  };
}
