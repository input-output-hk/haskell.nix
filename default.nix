{ sourcesOverride ? {}
, sources ? (import ./nix/sources.nix {}) // sourcesOverride
, pkgs ? (import sources."haskell.nix" {}).pkgs
, compiler-nix-name ? "ghc883"
}:
let
  project = pkgs.haskell-nix.cabalProject {
    inherit compiler-nix-name;
    src = pkgs.haskell-nix.haskellLib.cleanGit { src = ./.; name = "nix-tools"; };
    modules = [{ reinstallableLibGhc = true; }];
  };
in
  project // {
    shell = project.shellFor {
      tools = { cabal = "3.2.0.0"; };
      buildInputs = [
        pkgs.nix-prefetch-git
      ];
    };
  }

