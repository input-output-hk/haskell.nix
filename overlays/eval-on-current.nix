final: prev: {
  # This overlay makes `evalPackages` is like `buildPackages`, but on
  # `builtins.currentSystem`.
  # See `eval-on-build.nix` for alternative that just uses `buildPackages`.
  evalPackages = (import final.path (import ../. {
    inherit (final.haskell-nix) defaultCompilerNixName;
  }).nixpkgsArgs).buildPackages;
}