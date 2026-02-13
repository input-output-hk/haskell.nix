{
  description = "Lazy Input for Haskell.nix";

  inputs = {
    ghc9141 = {
      flake = false;
      url = "git+https://github.com/stable-haskell/ghc?ref=ghc-9.14.1-iog&submodules=1";
    };
  };

  outputs = inputs: inputs;
}
