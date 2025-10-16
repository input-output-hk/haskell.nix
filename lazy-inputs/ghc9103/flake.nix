{
  description = "Lazy Input for Haskell.nix";

  inputs = {
    ghc9103 = {
      flake = false;
      url = "git+https://github.com/ghc/ghc?rev=3f4d7d38b9661435bdde981451ac50c4335ed090&submodules=1";
    };
  };

  outputs = inputs: inputs;
}
