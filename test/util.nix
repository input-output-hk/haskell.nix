{ cabal-install }:

{
  # Add cabal as a buildInput for a haskell derivation. Useful for nix-shell.
  addCabalInstall = drv: drv.overrideAttrs (oldAttrs: {
    buildInputs = (oldAttrs.buildInputs or []) ++ [ cabal-install ];
  });
}
