{
  description = "Lazy Input for Haskell.nix";

  inputs = {
    ghc921 = {
      flake = false;
      url = "git+https://github.com/stable-haskell/ghc?ref=ghc-9.2.1-iog&submodules=1";
    };
  };

  outputs = inputs: inputs;
}
