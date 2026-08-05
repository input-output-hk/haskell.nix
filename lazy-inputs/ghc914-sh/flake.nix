{
  description = "Lazy Input for Haskell.nix";

  inputs = {
    ghc914-sh = {
      flake = false;
      # `stable-ghc-9.14-hn` is `stable-ghc-9.14` plus the two fixes the
      # cabalProject-built compilers need but do not get, because
      # overlays/bootstrap.nix's `onGhcjs` patch list only reaches the
      # hadrian-built ones: the JS backend's HEAP8/HEAPU8 emscripten exports
      # (cherry-picked from the fork's master, GHC #26290 -- without them every
      # ghcjs executable aborts in h$initEmscriptenHeap) and the Apple-ranlib
      # wasm padding fix for GHC.SysTools.Ar (no upstream equivalent).
      url = "git+https://github.com/stable-haskell/ghc?ref=stable-ghc-9.14-hn";
    };
  };

  outputs = inputs: inputs;
}
