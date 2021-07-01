final: prev: with prev;
  # sadly we need to patch GHC a bit.
   let
    ghcPkgOverrides = {
        enableIntegerSimple = false;
      };
    ghcDrvOverrides = drv: {
        hardeningDisable = (drv.hardeningDisable or []) ++ [ "stackprotector" "format" ] ++ lib.optionals prev.stdenv.hostPlatform.isAarch32 [ "pic" "pie" ];
      };
   in {
   haskell-nix = let
     # These patches (ghcPkgOverrides and ghcDrvOverrides) only apply to vanilla source ghcs.
     # Not ghcjs or binary distributions.
     # We also ignore ghc82. And are only concerned with ghc84+
     # we want to apply this only to non-ghcjs ones.
     # As we do some ghc <- ghcjs mapping for ghcjs.
     needsPatches = name:
       !(prev.stdenv.targetPlatform.isGhcjs or false)
       && lib.hasPrefix "ghc" name
       && !lib.hasPrefix "ghc82" name
       && !lib.hasPrefix "ghcjs" name
       && !lib.hasSuffix "Binary" name;
     overrideCompiler = compiler:
       ((compiler.override ghcPkgOverrides).overrideAttrs ghcDrvOverrides) // {
         dwarf = overrideCompiler compiler.dwarf;
       };
   in
     lib.recursiveUpdate prev.haskell-nix {
       compiler = lib.mapAttrs (_name: overrideCompiler)
         (lib.filterAttrs (name: _value: needsPatches name) prev.haskell-nix.compiler);
     };
   }
