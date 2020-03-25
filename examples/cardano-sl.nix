let inherit (import ../.) sources nixpkgsArgs;
    pkgs = import sources.nixpkgs-default nixpkgsArgs;
in (pkgs.haskell-nix.stackProject (import ./cardano-sl-args.nix)).cardano-sl.components.all
