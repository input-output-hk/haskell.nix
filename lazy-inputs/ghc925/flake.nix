{
  description = "Lazy Input for Haskell.nix";

  inputs = {
    ghc925 = {
      flake = false;
      url = "git+https://github.com/stable-haskell/ghc?ref=ghc-9.2.5-iog&submodules=1";
    };
  };

  outputs = inputs: inputs;
}
