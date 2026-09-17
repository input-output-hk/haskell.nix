{
  description = "Lazy Input for Haskell.nix";

  inputs = {
    ghc914-sh-cabal = {
      flake = false;
      url = "git+https://github.com/stable-haskell/Cabal.git?ref=stable-haskell/master";
    };
  };

  outputs = inputs: inputs;
}
