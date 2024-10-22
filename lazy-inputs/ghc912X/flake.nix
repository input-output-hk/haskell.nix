{
  description = "Lazy Input for Haskell.nix";

  inputs = {
    ghc912X = {
      flake = false;
      url = "git+https://gitlab.haskell.org/ghc/ghc?ref=ghc-9.12&submodules=1";
    };
  };

  outputs = inputs: inputs;
}
