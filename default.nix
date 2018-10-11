hackage: let haskell = {
  compat = import ./compat hackage;
  mkPkgSet = pkgs: pkg-def: import ./package-set.nix { inherit pkg-def pkgs hackage haskell; };
}; in haskell
