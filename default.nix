let haskellNix = {
      checkMaterialization ? false,  # Allows us to easily switch on materialization checking
      system ? builtins.currentSystem,
      sourcesOverride ? {},
      ... }@args: rec {
    sources = { nixpkgs-default = sources.nixpkgs; }
      // (import ./nix/sources.nix)
      // sourcesOverride;

    config   = import ./config.nix;
    overlays = [ allOverlays.combined ] ++ (
      if checkMaterialization == true
        then [(
          final: prev: {
            haskell-nix = prev.haskell-nix // {
              checkMaterialization = true;
            };
          }
        )]
        else []
    ) ++ (
      if args ? defaultCompilerNixName
        then [(
          final: prev: {
            haskell-nix = prev.haskell-nix // {
              userCompilerNixName = args.defaultCompilerNixName;
            };
          }
        )]
        else []
    ) ++ [(
      final: prev: {
        haskell-nix = prev.haskell-nix // {
          inherit overlays;
          sources = prev.haskell-nix.sources // sourcesOverride;
        };
      }
    )];
    allOverlays = import ./overlays args;
    nixpkgsArgs = { inherit config overlays system; };
    pkgs = import sources.nixpkgs-default nixpkgsArgs;
  };

  v1DeprecationMessage = "Version 1 is deprecated: use version 2 (nixpkgs arguments are available as the `nixpkgsArgs` attribute of version 2)";
  haskellNixV1 = (haskellNix {}).nixpkgsArgs // {
    overlays = builtins.trace v1DeprecationMessage (haskellNix {}).nixpkgsArgs.overlays;
  };
  haskellNixV2 = haskellNix;

# If no arguments, then you get V1
in haskellNixV1 // {
  __functor = _: {
      version ? 2,
      checkMaterialization ? false,  # Allows us to easily switch on materialization checking
      system ? builtins.currentSystem,
      sourcesOverride ? {},
      ... }@args:
    if version == 1
    then haskellNixV1
    else if version == 2
    then haskellNixV2 args
    else builtins.throw ("haskell.nix: unknown version: " + (builtins.toString version));
}
