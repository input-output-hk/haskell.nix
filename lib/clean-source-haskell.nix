# This function cleans common build products and files not needed to do a
# haskell build from a source directory.
# This can avoid unecessary builds when these files change.
# It has been copied from iohk-nix so that it can be used internally in
# haskell.nix and made available to projects that do not use iohk-nix.
# The original version is here:
# https://github.com/input-output-hk/iohk-nix/blob/c01bbd4e0c55a4101e2078d24046e0b2a731f871/clean-source-haskell.nix
{ pkgs }: src:
  let lib = pkgs.lib;
  in if (builtins.typeOf src) == "path"
    then lib.cleanSourceWith {
      filter = with pkgs.stdenv;
        name: type: let baseName = baseNameOf (toString name); in ! (
          # Filter out cabal build products.
          baseName == "dist" || baseName == "dist-newstyle" ||
          baseName == "cabal.project.local" ||
          lib.hasPrefix ".ghc.environment" baseName ||
          # Filter out stack build products.
          lib.hasPrefix ".stack-work" baseName ||
          # Filter out files which are commonly edited but don't
          # affect the cabal build.
          lib.hasSuffix ".nix" baseName
        );
      src = lib.cleanSource src;
    } else src
