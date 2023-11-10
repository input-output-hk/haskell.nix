final: prev: {
  haskell-nix = prev.haskell-nix // {
    compilerNameMap = prev.haskell-nix.compilerNameMap //
      builtins.listToAttrs (final.lib.mapAttrsToList (name: value: {
        name = name + "llvm";
        value = value + "llvm";
      }) prev.haskell-nix.compilerNameMap);

    compiler = prev.haskell-nix.compiler //
      builtins.listToAttrs (final.lib.mapAttrsToList (name: ghc: {
        name = name + "llvm";
        value = ghc.override { useLLVM = true; };
      }) prev.haskell-nix.compiler);
  };
}

