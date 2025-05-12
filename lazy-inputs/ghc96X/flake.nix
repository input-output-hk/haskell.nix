{
  description = "Lazy Input for Haskell.nix";

  inputs = {
    ghc96X = {
      flake = false;
      url = "git+https://gitlab.haskell.org/ghc/ghc?ref=ghc-9.6&submodules=1";
    };
  };

  outputs = inputs: inputs;
}
