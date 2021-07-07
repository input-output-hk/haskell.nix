final: prev: {
  # This overlay makes `evalPackages` is like `buildPackages`, but on
  # `builtins.currentSystem` (when not building a nix flake).
  # We do not pass the `config` or `overlays` (not in `final.haskell-nix.overlays`).
  # This could cause flake and non flake builds to diverge, although only the
  # derivations that use `evalPackages` (these should be eval time only derviations
  # used to build nix inputs for IFD, the generated nix should match and so derivations
  # that depend on the IFD should match).
  evalPackages =
    # If we are building a flake there will be no currentSystem attribute
    if builtins ? currentSystem
      then (import final.path {
          inherit (final.haskell-nix) overlays;
        }).buildPackages
      else final.buildPackages;
}