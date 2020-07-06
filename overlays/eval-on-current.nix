final: prev: {
  # This overlay makes `evalPackages` is like `buildPackages`, but on
  # `builtins.currentSystem`.
  # See `eval-on-build.nix` for alternative that just uses `buildPackages`.
  evalPackages = (import final.path {
    inherit (final.haskell-nix) overlays;
  }).buildPackages;
}