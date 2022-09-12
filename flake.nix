{
  description = "Alternative Haskell Infrastructure for Nixpkgs";

  inputs = {
    nixpkgs.follows = "nixpkgs-unstable";
    nixpkgs-2003 = { url = "github:NixOS/nixpkgs/nixpkgs-20.03-darwin"; };
    nixpkgs-2105 = { url = "github:NixOS/nixpkgs/nixpkgs-21.05-darwin"; };
    nixpkgs-2111 = { url = "github:NixOS/nixpkgs/nixpkgs-21.11-darwin"; };
    nixpkgs-2205 = { url = "github:NixOS/nixpkgs/nixpkgs-22.05-darwin"; };
    nixpkgs-unstable = { url = "github:NixOS/nixpkgs/nixpkgs-unstable"; };
    flake-compat = { url = "github:input-output-hk/flake-compat"; flake = false; };
    flake-utils = { url = "github:numtide/flake-utils"; };
    hydra.url = "hydra";
    hackage = {
      url = "github:input-output-hk/hackage.nix";
      flake = false;
    };
    stackage = {
      url = "github:input-output-hk/stackage.nix";
      flake = false;
    };
    cabal-32 = {
      url = "github:haskell/cabal/3.2";
      flake = false;
    };
    cabal-34 = {
      url = "github:haskell/cabal/3.4";
      flake = false;
    };
    cabal-36 = {
      url = "github:haskell/cabal/3.6";
      flake = false;
    };
    cardano-shell = {
      url = "github:input-output-hk/cardano-shell";
      flake = false;
    };
    "ghc-8.6.5-iohk" = {
      type = "github";
      owner = "input-output-hk";
      repo = "ghc";
      ref = "release/8.6.5-iohk";
      flake = false;
    };
    hpc-coveralls = {
      url = "github:sevanspowell/hpc-coveralls";
      flake = false;
    };
    nix-tools = {
      url = "github:input-output-hk/nix-tools";
      flake = false;
    };
    old-ghc-nix = {
      url = "github:angerman/old-ghc-nix/master";
      flake = false;
    };
    HTTP = {
      url = "github:phadej/HTTP";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, nixpkgs-unstable, nixpkgs-2105, nixpkgs-2111, nixpkgs-2205, flake-utils, ... }@inputs:
    let compiler = "ghc924";
      config = import ./config.nix;
    in {
      inherit config;
      overlay = self.overlays.combined;
      overlays = import ./overlays { sources = inputs; };
      internal = rec {
        nixpkgsArgs = {
          inherit config;
          overlays = [ self.overlay ];
        };

        sources = inputs;

        overlaysOverrideable = import ./overlays;
        # Compatibility with old default.nix
        compat = { checkMaterialization ?
            false # Allows us to easily switch on materialization checking
          , system, sourcesOverride ? { }, ... }@args: rec {
            sources = inputs // sourcesOverride;
            allOverlays = import ./overlays (args // { inherit sources; });
            inherit config;
            # We are overriding 'overlays' and 'nixpkgsArgs' from the
            # flake outputs so that we can incorporate the args passed
            # to the compat layer (e.g. sourcesOverride).
            overlays = [ allOverlays.combined ]
              ++ (if checkMaterialization == true then
                [
                  (final: prev: {
                    haskell-nix = prev.haskell-nix // {
                      checkMaterialization = true;
                    };
                  })
                ]
              else
                [ ]);
            nixpkgsArgs = {
              inherit config overlays;
            };
            pkgs = import nixpkgs
              (nixpkgsArgs // { localSystem = { inherit system; }; });
            pkgs-2105 = import nixpkgs-2105
              (nixpkgsArgs // { localSystem = { inherit system; }; });
            pkgs-2111 = import nixpkgs-2111
              (nixpkgsArgs // { localSystem = { inherit system; }; });
            pkgs-2205 = import nixpkgs-2205
              (nixpkgsArgs // { localSystem = { inherit system; }; });
            pkgs-unstable = import nixpkgs-unstable
              (nixpkgsArgs // { localSystem = { inherit system; }; });
            hix = import ./hix/default.nix { inherit pkgs; };
          };
      };

      # Note: `nix flake check` evaluates outputs for all platforms, and haskell.nix
      # uses IFD heavily, you have to have the ability to build for all platforms
      # supported by haskell.nix, e.g. with remote builders, in order to check this flake.
      # If you want to run the tests for just your platform, run `./test/tests.sh` or
      # `nix-build -A checks.$PLATFORM`
    } // flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" "aarch64-linux" "aarch64-darwin" ] (system: rec {
      legacyPackages = (self.internal.compat { inherit system; }).pkgs;
      legacyPackagesUnstable = (self.internal.compat { inherit system; }).pkgs-unstable;

      # FIXME: Currently `nix flake check` requires `--impure` because coverage-golden
      # (and maybe other tests) import projects that use builtins.currentSystem
      checks = builtins.listToAttrs (map (pkg: {
        name = pkg.name;
        value = pkg;
      }) (nixpkgs.lib.collect nixpkgs.lib.isDerivation (import ./test rec {
        haskellNix = self.internal.compat { inherit system; };
        compiler-nix-name = compiler;
        pkgs = haskellNix.pkgs;
      })));
      # Exposed so that buildkite can check that `allow-import-from-derivation=false` works for core of haskell.nix
      roots = legacyPackagesUnstable.haskell-nix.roots compiler;

      packages = ((self.internal.compat { inherit system; }).hix).apps;

      devShells.default = with self.legacyPackages.${system};
        mkShell {
          buildInputs = [
            nixUnstable
            cabal-install
            haskell-nix.compiler.${compiler}
            haskell-nix.nix-tools.${compiler}
          ];
        };
    });

  # --- Flake Local Nix Configuration ----------------------------
  nixConfig = {
    # This sets the flake to use the IOG nix cache.
    # Nix should ask for permission before using it,
    # but remove it here if you do not want it to.
    extra-substituters = ["https://cache.iog.io"];
    extra-trusted-public-keys = ["hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="];
    allow-import-from-derivation = "true";
  };
}
