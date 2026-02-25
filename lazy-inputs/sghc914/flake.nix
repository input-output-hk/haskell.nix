{
  description = "Lazy Input for Haskell.nix";

  inputs = {
    sghc914 = {
      flake = false;
      url = "git+https://github.com/stable-haskell/ghc?ref=stable-ghc-9.14&submodules=1";
    };
  };

  outputs = inputs: inputs;
}
