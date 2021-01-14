{ checkMaterialization ? false  # Allows us to easily switch on materialization checking
, system ? builtins.currentSystem
, sourcesOverride ? {}
, ... }@args: rec {
  sources  = (import ./nix/sources.nix) // sourcesOverride;
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
}
