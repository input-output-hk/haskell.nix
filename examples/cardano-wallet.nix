with import ../nixpkgs (import ../.);
(haskell-nix.stackProject (import ./cardano-wallet-args.nix))
      .cardano-wallet.components.all
