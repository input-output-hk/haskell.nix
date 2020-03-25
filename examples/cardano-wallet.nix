let inherit (import ../. {}) sources nixpkgsArgs;
    pkgs = import sources.nixpkgs-default nixpkgsArgs;
in (pkgs.haskell-nix.stackProject (import ./cardano-wallet-args.nix)).cardano-wallet.components.all
