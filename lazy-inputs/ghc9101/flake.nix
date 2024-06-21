{
  description = "Lazy Input for Haskell.nix";

  inputs = {
    ghc9101 = {
      flake = false;
      url = "git+https://github.com/stable-haskell/ghc?ref=ghc-9.10.1-iog&submodules=1";
    };
  };

  outputs = inputs: inputs;
}
