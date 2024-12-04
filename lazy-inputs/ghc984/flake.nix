{
  description = "Lazy Input for Haskell.nix";

  inputs = {
    ghc984 = {
      flake = false;
      url = "git+https://github.com/stable-haskell/ghc?ref=ghc-9.8.4-iog&submodules=1";
    };
  };

  outputs = inputs: inputs;
}
