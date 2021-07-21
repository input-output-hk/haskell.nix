{ compiler-nix-name ? "ghc8105"
}:
let
  sources = import ./nix/sources.nix {};
  haskellNix = import sources."haskell.nix" {};
  pkgs = import haskellNix.sources.nixpkgs-unstable haskellNix.nixpkgsArgs;
in
  pkgs.haskell-nix.cabalProject {
    inherit compiler-nix-name;
    src = pkgs.haskell-nix.haskellLib.cleanGit { src = ./.; name = "nix-tools"; };
    shell = {
      tools.cabal = {};
      buildInputs = [
        pkgs.nix-prefetch-git
      ];
    };
  }

