final: prev: {
  haskell = prev.haskell // {
      compiler = __mapAttrs (name: ghc: final.haskell-nix.haskellLib.makeCompilerDeps ghc // {
        inherit (final.haskell-nix.compiler.${name}) raw-src configured-src;
    }) prev.haskell.compiler;
  };
}
