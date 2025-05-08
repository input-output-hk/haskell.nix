{
  description = "Lazy Input for Haskell.nix";

  inputs = {
    ghc9102 = {
      flake = false;
      url = "git+https://github.com/stable-haskell/ghc?ref=ghc-9.10.2-iog&submodules=1";
    };
  };

  outputs = inputs: inputs;
}
