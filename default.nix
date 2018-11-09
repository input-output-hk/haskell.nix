hackage: let haskell = {
  compat = import ./compat hackage;
  mkPkgSet = pkgs: pkg-def: import ./package-set.nix { inherit pkg-def pkgs hackage haskell; };
  # The *new* pkg set is one that build components.
  # This also uses the module system for much greater extensibility.
  # To make extend and override things, pass a modules argument to new-package-set.nix
  mkNewPkgSet = { pkgs, pkg-def, modules ? [] }@args: import ./new-package-set.nix (args // { inherit hackage; });
}; in haskell
