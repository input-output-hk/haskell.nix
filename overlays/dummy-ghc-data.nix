final: prev: {
  haskell-nix = prev.haskell-nix // {
      compiler = __mapAttrs (name:
        final.haskell-nix.haskellLib.makeDummyGhcData ../materialized) prev.haskell-nix.compiler;
  };
  haskell = prev.haskell // {
      compiler = __mapAttrs (name:
        final.haskell-nix.haskellLib.makeDummyGhcData ../materialized/nixpkgs) prev.haskell.compiler;
  };
}
