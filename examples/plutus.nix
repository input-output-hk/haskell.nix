let inherit (import ../. {}) sources nixpkgsArgs;
    pkgs = import sources.nixpkgs nixpkgsArgs;
in (pkgs.haskell-nix.stackProject (import ./plutus-args.nix)).language-plutus-core.components.all
