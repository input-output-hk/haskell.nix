with import ../nixpkgs (import ../.);
(haskell-nix.stackProject (import ./plutus-args.nix))
      .language-plutus-core.components.all
