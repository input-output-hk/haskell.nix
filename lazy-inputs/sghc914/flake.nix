{
  description = "Lazy Input for Haskell.nix";

  inputs = {
    sghc914 = {
      flake = false;
      # Pinned to the RTS THREADED_RTS-in-Cmm fix branch until it merges to
      # stable-ghc-9.14 (stable-haskell/ghc#191, hkm/rts-threaded-cmm-optc).
      url = "git+https://github.com/stable-haskell/ghc?ref=hkm/rts-threaded-cmm-optc";
    };
  };

  outputs = inputs: inputs;
}
