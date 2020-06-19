{ checkMaterialization ? false  # Allows us to easily switch on materialization checking
, defaultCompilerNixName ? null # Quick way to override the default compiler e.g. "ghc883"
, system ? builtins.currentSystem
, sourcesOverride ? {}
, ... }@args: rec {
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
}
