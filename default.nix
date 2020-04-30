let haskellNix = rec {
    sources = {
      inherit (import ./nixpkgs/default.nix) nixpkgs-1903 nixpkgs-1909 nixpkgs-default;
    };

    config   = import ./config.nix;
    overlays = [ allOverlays.combined ];
    allOverlays = import ./overlays;
    nixpkgsArgs = { inherit config overlays; };
  };

  haskellNixV1 = haskellNix.nixpkgsArgs;
  haskellNixV2 = haskellNix;

  v1DeprecationMessage = "Version 1 is deprecated: use version 2 (nixpkgs arguments are available as the `nixpkgsArgs` attribute of version 2)";
# If no arguments, then you get V1
# I'd like to make importing directly issue a warning, but I couldn't figure out a way to make it happen
in haskellNixV1 // {
  __functor = _: { version ? 2 }:
    if version == 1
    then builtins.trace v1DeprecationMessage haskellNixV1
    else if version == 2
    then haskellNixV2
    else builtins.throw ("haskell.nix: unknown version: " + (builtins.toString version));
}
