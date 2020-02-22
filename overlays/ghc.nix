self: super: with super;
  # sadly we need to patch GHC a bit.
   let
    ghcPkgOverrides = {
        # With nixpkgs 19.09 we seem to have to explicity disable shared
        # when cross compiline for windows, otherwise we get:
        #   utils/deriveConstants/ghc.mk:28: *** Do not know how to prependLibraryPath on Windows.  Stop.
        enableShared = !super.stdenv.targetPlatform.isWindows;
        enableIntegerSimple = false;
      } // lib.optionalAttrs self.stdenv.hostPlatform.isAarch32 {
        enableRelocatableStaticLibs = false;
      };
    ghcDrvOverrides = drv: {
        hardeningDisable = (drv.hardeningDisable or []) ++ [ "stackprotector" "format" ] ++ lib.optionals super.stdenv.hostPlatform.isAarch32 [ "pic" "pie" ];
      };
   in {
   haskell-nix = let
     # These patches (ghcPkgOverrides and ghcDrvOverrides) only apply to vanilla source ghcs.
     # Not ghcjs or binary distributions.
     # We also ignore ghc82. And are only concerned with ghc84+
     # we want to apply this only to non-ghcjs ones.
     # As we do some ghc <- ghcjs mapping for ghcjs.
     needsPatches = name:
       !(super.stdenv.targetPlatform.isGhcjs or false)
       && lib.hasPrefix "ghc" name
       && !lib.hasPrefix "ghc82" name
       && !lib.hasPrefix "ghcjs" name
       && !lib.hasSuffix "Binary" name;
     overrideCompiler = compiler:
       (compiler.override ghcPkgOverrides).overrideAttrs ghcDrvOverrides;
   in
     lib.recursiveUpdate super.haskell-nix {
       compiler = lib.mapAttrs (_name: overrideCompiler)
         (lib.filterAttrs (name: _value: needsPatches name) super.haskell-nix.compiler);
     };
   }
