let inherit (import ../. {}) sources nixpkgsArgs;
    pkgs = import sources.nixpkgs (nixpkgsArgs // { crossSystem.config = "x86_64-pc-mingw32"; });
in (pkgs.haskell-nix.stackProject (import ./plutus-args.nix)).language-plutus-core.components.all
