{
  description = "Lazy Input for Haskell.nix";

  inputs = {
    ghc9103 = {
      flake = false;
      url = "git+https://github.com/stable-haskell/ghc?ref=ghc-9.10.3-iog&submodules=1";
    };
  };

  outputs = inputs: inputs;
}
