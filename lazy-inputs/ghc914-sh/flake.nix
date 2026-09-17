{
  description = "Lazy Input for Haskell.nix";

  inputs = {
    ghc914-sh = {
      flake = false;
      # `stable-ghc-9.14-hn` is `stable-ghc-9.14` plus a handful of fixes.
      #
      # Two of them the cabalProject-built compilers need but do not get,
      # because overlays/bootstrap.nix's `onGhcjs` patch list only reaches the
      # hadrian-built ones: the JS backend's HEAP8/HEAPU8 emscripten exports
      # (cherry-picked from the fork's master, GHC #26290 -- without them every
      # ghcjs executable aborts in h$initEmscriptenHeap) and the Apple-ranlib
      # wasm padding fix for GHC.SysTools.Ar (no upstream equivalent).
      #
      # The third is a plain fork bug, and applies to every build of it:
      # splitting the rts into sub-libraries also dropped the way suffix from
      # *every* package's library name, not just the rts's.  Profiling is not
      # an rts-only way, so `-prof` links stopped asking for `libHSfoo_p.a`,
      # silently picked up the vanilla archives instead -- which resolve every
      # ordinary symbol -- and failed on `pushCostCentre` and friends.
      url = "git+https://github.com/stable-haskell/ghc?ref=stable-ghc-9.14-hn";
    };
  };

  outputs = inputs: inputs;
}
