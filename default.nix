{ compiler-nix-name ? "ghc8104"
}:
let
  sources = import ./nix/sources.nix {};
  haskellNix = import sources."haskell.nix" {};
  pkgs = import haskellNix.sources.nixpkgs-unstable haskellNix.nixpkgsArgs;
  project = pkgs.haskell-nix.cabalProject {
    inherit compiler-nix-name;
    src = pkgs.haskell-nix.haskellLib.cleanGit { src = ./.; name = "nix-tools"; };
  };
in
  project // {
    shell = project.shellFor {
      tools = { cabal = "latest"; };
      buildInputs = [
        pkgs.nix-prefetch-git
      ];
    };
  }

