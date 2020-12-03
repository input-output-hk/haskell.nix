{ sourcesOverride ? {}, compiler-nix-name ? "ghc883" }:
(import ./. { inherit sourcesOverride compiler-nix-name; }).shell
    
