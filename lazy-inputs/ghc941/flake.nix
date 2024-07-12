{
  description = "Lazy Input for Haskell.nix";

  inputs = {
    ghc941 = {
      flake = false;
      url = "git+https://github.com/stable-haskell/ghc?ref=ghc-9.4.1-iog&submodules=1";
    };
  };

  outputs = inputs: inputs;
}
