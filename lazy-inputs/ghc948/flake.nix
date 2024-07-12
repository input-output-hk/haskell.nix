{
  description = "Lazy Input for Haskell.nix";

  inputs = {
    ghc948 = {
      flake = false;
      url = "git+https://github.com/stable-haskell/ghc?ref=ghc-9.4.8-iog&submodules=1";
    };
  };

  outputs = inputs: inputs;
}
