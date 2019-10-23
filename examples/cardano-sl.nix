with import ../nixpkgs (import ../.);
(haskell-nix.stackProject (import ./cardano-sl-args.nix))
      .cardano-sl.components.all
