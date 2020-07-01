final: prev: {
  # This overlay makes `evalPackages` is like `buildPackages`, but on
  # `builtins.currentSystem`.
  # See `eval-on-build.nix` for alternative that just uses `buildPackages`.
  evalPackages = (import final.path (import ../. ({
      # If check materialization was turned on also check in eval packages
      inherit (final.haskell-nix) checkMaterialization;
      # Forward any sourcesOverrides by simply passing all the sources as sourcesOverrides
      sourcesOverride = final.haskell-nix.sources;
    } //
      # Forward user specified defaultCompilerNixName
      final.lib.optionalAttrs (final.haskell-nix ? userCompilerNixName) {
        defaultCompilerNixName = final.haskell-nix.userCompilerNixName;
      }
    )).nixpkgsArgs).buildPackages;
}