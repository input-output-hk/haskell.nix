{ checkMaterialization ? false  # Allows us to easily switch on materialization checking
, configOverride ? {}
, system ? builtins.currentSystem
, sourcesOverride ? {}
, ... }@args: rec {
  sources  = (import ./nix/sources.nix) // sourcesOverride;
  config   = (import ./config.nix) // configOverride;
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
  pkgs = import sources.nixpkgs nixpkgsArgs;
  pkgs-unstable = import sources.nixpkgs-unstable nixpkgsArgs;
}
