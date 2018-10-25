hackage: let haskell = {
  compat = import ./compat hackage;
  mkPkgSet = pkgs: pkg-def: import ./package-set.nix { inherit pkg-def pkgs hackage haskell; };
  # The *new* pkg set is one that build components.
  mkNewPkgSet = pkgs: pkg-def: import ./new-package-set.nix { inherit pkgs hackage; planFunc = pkg-def; };
}; in haskell
