let
  # pkg-def's may reference boot packages, but those
  # are not guaranteed to be available on hackage, as
  # it is a manual process.  They might eventually show
  # up much later on hackage; but are not installable
  # anyway. Therefore we just strip them out of the
  # pkg-def's packages.
  boot-pkgs = [ "rts" "ghc" "ghci" "ghc-boot" "ghc-boot-th"
                "ghc-heap" # since ghc 8.6.
              ];
  strip-pkg-def = pkgs: pkg-def: hackage: with pkgs.lib;
    mapAttrs (k: v: if k == "packages"
                    then filterAttrs (k: _: !(builtins.elem k boot-pkgs)) v
                    else v)
             (pkg-def hackage);
in
hackage: let haskell = rec {
  # ghc hackage patches.
  # these are patches that turn hackage packages into the same as the ones
  # ghc ships with the supposedly same version. See GHC Track Issue: 16199
  ghcHackagePatches = import ./patches;

  compat = import ./compat.nix;

  mkPkgSet
    = { pkgs, pkg-def, pkg-def-overlays ? [], modules ? [] }@args:
      import ./package-set.nix (args // { inherit hackage; pkg-def = strip-pkg-def pkgs pkg-def; });

  mkNewPkgSet = args: builtins.trace "DEPRECATED: use mkPkgSet instead of mkNewPkgSet" (mkPkgSet args);

}; in haskell
