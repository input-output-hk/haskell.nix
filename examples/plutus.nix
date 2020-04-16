let inherit (import ../. {}) sources nixpkgsArgs;
    pkgs = import sources.nixpkgs-default nixpkgsArgs;
in (pkgs.haskell-nix.stackProject (import ./plutus-args.nix)).language-plutus-core.components.all
