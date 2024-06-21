{
  description = "Lazy Input for Haskell.nix";

  inputs = {
    ghc965 = {
      flake = false;
      url = "git+https://github.com/stable-haskell/ghc?ref=ghc-9.6.5-iog&submodules=1";
    };
  };

  outputs = inputs: inputs;
}
