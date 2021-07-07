final: prev: {
  # This overlay makes `evalPackages` is like `buildPackages`, but on
  # `builtins.currentSystem`.
  evalPackages =
    if builtins ? currentSystem
      then (import final.path {
          inherit (final.haskell-nix) overlays;
        }).buildPackages
      else final.buildPackages;
}