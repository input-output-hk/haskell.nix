with import ../nixpkgs ((import ../.) // { crossSystem.config = "x86_64-pc-mingw32"; });
(haskell-nix.stackProject (import ./cardano-sl-args.nix))
      .cardano-sl.components.all