{
  # This is a boilerplate `flake.nix` file used by
  # `hix develop`, `hix flake`, `hix build` and `hix run`.
  description = "Default hix flake";
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.projectArgs.url = "github:input-output-hk/haskell.nix?dir=hix/empty-set";
  inputs.projectArgs.flake = false;
  inputs.src.flake = false;
  outputs = { self, src, nixpkgs, flake-utils, haskellNix, projectArgs }:
    flake-utils.lib.eachSystem [ "EVAL_SYSTEM" ] (system:
    let
      overlays = [ haskellNix.overlay
        (final: _prev: {
          hixProject =
            final.haskell-nix.hix.project ({
              inherit src;
            } // import projectArgs);
        })
      ];
      pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
      flake = pkgs.hixProject.flake {};
    in flake // {
      legacyPackages = pkgs;
    });

  # --- Flake Local Nix Configuration ----------------------------
  nixConfig = {
    extra-substituters = ["https://cache.iog.io"];
    extra-trusted-public-keys = ["hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="];
    allow-import-from-derivation = "true";
  };
}
