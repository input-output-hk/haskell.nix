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
