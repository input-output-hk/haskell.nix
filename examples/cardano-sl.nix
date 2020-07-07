let inherit (import ../. {}) sources nixpkgsArgs;
    pkgs = import sources.nixpkgs nixpkgsArgs;
in (pkgs.haskell-nix.stackProject (import ./cardano-sl-args.nix)).cardano-sl.components.all
