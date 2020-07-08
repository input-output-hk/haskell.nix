let inherit (import ../. {}) sources nixpkgsArgs;
    pkgs = import sources.nixpkgs nixpkgsArgs;
in (pkgs.haskell-nix.stackProject (import ./cardano-wallet-args.nix)).cardano-wallet.components.all
