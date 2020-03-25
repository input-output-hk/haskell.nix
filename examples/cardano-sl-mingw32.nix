let inherit (import ../.) sources nixpkgsArgs;
    pkgs = import sources.nixpkgs-default (nixpkgsArgs // { crossSystem.config = "x86_64-pc-mingw32"; });
in (pkgs.haskell-nix.stackProject (import ./cardano-sl-args.nix)).cardano-sl.components.all
