{
  description = "Lazy Input for Haskell.nix";

  inputs = {
    ghc914X = {
      flake = false;
      url = "git+https://gitlab.haskell.org/ghc/ghc?ref=ghc-9.14&submodules=1";
    };
  };

  outputs = inputs: inputs;
}
