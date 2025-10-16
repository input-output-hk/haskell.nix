{
  pkgs = hackage:
    {
      packages = {
        ghc-bignum.revision = hackage.ghc-bignum."1.3".revisions.default;
        transformers.revision = import ./cabal-files/transformers.nix;
        deepseq.revision = hackage.deepseq."1.4.8.1".revisions.default;
        mtl.revision = import ./cabal-files/mtl.nix;
        base.revision = hackage.base."4.18.2.0".revisions.default;
        array.revision = hackage.array."0.5.6.0".revisions.default;
        happy.revision = import ./cabal-files/happy.nix;
        template-haskell.revision = hackage.template-haskell."2.20.0.0".revisions.default;
        ghc-boot-th.revision = hackage.ghc-boot-th."9.6.4".revisions.default;
        ghc-prim.revision = hackage.ghc-prim."0.10.0".revisions.default;
        pretty.revision = hackage.pretty."1.1.3.6".revisions.default;
        containers.revision = hackage.containers."0.6.7".revisions.default;
      };
      compiler = {
        version = "9.6.4";
        nix-name = "ghc964";
        packages = {
          "ghc-boot-th" = "9.6.4";
          "pretty" = "1.1.3.6";
          "array" = "0.5.6.0";
          "ghc-prim" = "0.10.0";
          "template-haskell" = "2.20.0.0";
          "ghc-bignum" = "1.3";
          "deepseq" = "1.4.8.1";
          "containers" = "0.6.7";
          "base" = "4.18.2.0";
        };
      };
    };
  extras = hackage:
    { packages = {}; };
  modules = [
    {
      preExistingPkgs = [
        "ghc-bignum"
        "deepseq"
        "base"
        "array"
        "template-haskell"
        "ghc-boot-th"
        "ghc-prim"
        "pretty"
        "containers"
      ];
    }
    ({ lib, ... }:
      { packages = {}; })
    ({ lib, ... }:
      {
        packages = {
          "template-haskell".components.library.planned = lib.mkOverride 900 true;
          "transformers".components.library.planned = lib.mkOverride 900 true;
          "array".components.library.planned = lib.mkOverride 900 true;
          "containers".components.library.planned = lib.mkOverride 900 true;
          "deepseq".components.library.planned = lib.mkOverride 900 true;
          "happy".components.exes."happy".planned = lib.mkOverride 900 true;
          "ghc-bignum".components.library.planned = lib.mkOverride 900 true;
          "pretty".components.library.planned = lib.mkOverride 900 true;
          "mtl".components.library.planned = lib.mkOverride 900 true;
          "ghc-boot-th".components.library.planned = lib.mkOverride 900 true;
          "base".components.library.planned = lib.mkOverride 900 true;
          "ghc-prim".components.library.planned = lib.mkOverride 900 true;
        };
      })
  ];
}