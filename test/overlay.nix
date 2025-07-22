final: prev: {
  haskell-nix = prev.haskell-nix // {
    extraPkgconfigMappings = prev.haskell-nix.extraPkgconfigMappings or {} // {
      "libsodium" = [ "libsodium-18" ];
    };
  };
  libsodium-18 = (final.callPackage (final.haskell-nix.sources.nixpkgs-2311 + "/pkgs/development/libraries/libsodium") {}).overrideAttrs (_: { dontDisableStatic = true; });
}
