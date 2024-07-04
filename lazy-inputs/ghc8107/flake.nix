{
  description = "Lazy Input for Haskell.nix";

  inputs = {
    ghc8107 = {
      flake = false;
      url = "git+https://github.com/stable-haskell/ghc?ref=ghc-8.10.7-iog&submodules=1";
    };
  };

  outputs = inputs: inputs;
}
