{
  description = "Lazy Input for Haskell.nix";

  inputs = {
    ghc914-sh = {
      flake = false;
      url = "git+https://github.com/stable-haskell/ghc?ref=stable-ghc-9.14";
    };
  };

  outputs = inputs: inputs;
}
