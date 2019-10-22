with import ../nixpkgs ((import ../.) // { crossSystem.config = "x86_64-pc-mingw32"; });
(haskell-nix.stackProject (import ./plutus-args.nix))
      .language-plutus-core.components.all
