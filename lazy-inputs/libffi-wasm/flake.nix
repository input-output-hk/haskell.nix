{
  description = "Lazy Input for Haskell.nix";

  inputs = {
    libffi-wasm = {
      flake = false;
      url = "gitlab:haskell-wasm/libffi-wasm?host=gitlab.haskell.org";
    };
  };

  outputs = inputs: inputs;
}
