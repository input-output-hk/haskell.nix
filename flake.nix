{
  description = "Alternative Haskell Infrastructure for Nixpkgs";

  inputs = {
    nixpkgs = { url = "github:NixOS/nixpkgs/nixpkgs-20.09-darwin"; };
    nixpkgs-2003 = { url = "github:NixOS/nixpkgs/nixpkgs-20.03-darwin"; };
    nixpkgs-2009 = { url = "github:NixOS/nixpkgs/nixpkgs-20.09-darwin"; };
    nixpkgs-2105 = { url = "github:NixOS/nixpkgs/nixpkgs-21.05-darwin"; };
    nixpkgs-unstable = { url = "github:NixOS/nixpkgs/nixpkgs-unstable"; };
    flake-utils = { url = "github:numtide/flake-utils"; };
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
      url = "github:angerman/old-ghc-nix/master2";
      flake = false;
    };
    HTTP = {
      url = "github:phadej/HTTP";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, nixpkgs-unstable, flake-utils, ... }@inputs:
    let compiler = "ghc884";
    in {
      overlay = self.overlays.combined;
      overlays = import ./overlays { sources = inputs; };
      internal = rec {
        config = import ./config.nix;
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
            allOverlays = import ./overlays args;
            inherit config nixpkgsArgs;
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
            pkgs = import nixpkgs
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
    } // flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system: {
      legacyPackages = (self.internal.compat { inherit system; }).pkgs;

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

      devShell = with self.legacyPackages.${system};
        mkShell {
          buildInputs = [
            nixUnstable
            cabal-install
            haskell-nix.compiler.${compiler}
            haskell-nix.nix-tools.${compiler}
          ];
        };
    });
}
