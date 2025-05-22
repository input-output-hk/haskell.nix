{
  description = "Lazy Input for Haskell.nix";

  inputs = {
    ghc9121 = {
      flake = false;
      url = "git+https://github.com/stable-haskell/ghc?ref=ghc-9.12.1-iog&submodules=1";
    };
  };

  outputs = inputs: inputs;
}
