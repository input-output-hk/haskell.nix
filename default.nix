let haskellNix = {
      checkMaterialization ? false,  # Allows us to easily switch on materialization checking
      defaultCompilerNixName ? null, # Quick way to override the default compiler e.g. "ghc883"
      system ? builtins.currentSystem,
      sourcesOverride ? {},
      ... }@args: rec {
    sources = (import ./nix/sources.nix) // sourcesOverride;

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
      if defaultCompilerNixName != null
        then [(
          final: prev: {
            haskell-nix = prev.haskell-nix // {
              inherit defaultCompilerNixName;
            };
          }
        )]
        else []
    );
    allOverlays = import ./overlays args;
    nixpkgsArgs = { inherit config overlays system; };
    pkgs = import sources.nixpkgs-default nixpkgsArgs;
  };

  haskellNixV1 = (haskellNix {}).nixpkgsArgs;
  haskellNixV2 = haskellNix;

  v1DeprecationMessage = "Version 1 is deprecated: use version 2 (nixpkgs arguments are available as the `nixpkgsArgs` attribute of version 2)";
# If no arguments, then you get V1
# I'd like to make importing directly issue a warning, but I couldn't figure out a way to make it happen
in haskellNixV1 // {
  __functor = _: { version ? 2, ... }@args:
    if version == 1
    then builtins.trace v1DeprecationMessage haskellNixV1
    else if version == 2
    then haskellNixV2 args
    else builtins.throw ("haskell.nix: unknown version: " + (builtins.toString version));
}
