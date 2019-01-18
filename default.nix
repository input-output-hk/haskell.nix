let
  # pkg-def's may reference boot packages, but those
  # are not guaranteed to be available on hackage, as
  # it is a manual process.  They might eventually show
  # up much later on hackage; but are not installable
  # anyway. Therefore we just strip them out of the
  # pkg-def's packages.
  boot-pkgs = [ "rts" "ghc" "ghci" "ghc-boot" "ghc-boot-th" ];
  strip-pkg-def = pkgs: pkg-def: hackage: with pkgs.lib;
    mapAttrs (k: v: if k == "packages"
                    then filterAttrs (k: _: !(builtins.elem k boot-pkgs)) v
                    else v)
             (pkg-def hackage);
in
hackage: let haskell = {
  # ghc hackage patches.
  # these are patches that turn hackage packages into the same as the ones
  # ghc ships with the supposedly same version. See GHC Track Issue: 16199
  ghcHackagePatches = import ./patches;

  compat = import ./compat hackage;
  mkPkgSet = pkgs: pkg-def: import ./package-set.nix { inherit pkgs hackage haskell; pkg-def = strip-pkg-def pkgs pkg-def; };
  # The *new* pkg set is one that build components.
  # This also uses the module system for much greater extensibility.
  # To make extend and override things, pass a modules argument to new-package-set.nix
  mkNewPkgSet
    = { pkgs, pkg-def, pkg-def-overlays ? [], modules ? [] }@args:
      import ./new-package-set.nix (args // { inherit hackage; pkg-def = strip-pkg-def pkgs pkg-def; });
}; in haskell
