{
  description = "Lazy Input for Haskell.nix";

  inputs = {
    ghc914X = {
      flake = false;
      url = "git+https://github.com/stable-haskell/ghc?ref=stable-ghc-9.14&submodules=1";
    };
  };

  outputs = inputs: inputs;
}
