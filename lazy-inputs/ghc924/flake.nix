{
  description = "Lazy Input for Haskell.nix";

  inputs = {
    ghc924 = {
      flake = false;
      url = "git+https://github.com/stable-haskell/ghc?ref=ghc-9.2.4-iog&submodules=1";
    };
  };

  outputs = inputs: inputs;
}
