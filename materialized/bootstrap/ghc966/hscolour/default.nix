{
  pkgs = hackage:
    {
      packages = {
        ghc-prim.revision = hackage.ghc-prim."0.10.0".revisions.default;
        base.revision = hackage.base."4.18.2.1".revisions.default;
        hscolour.revision = import ./cabal-files/hscolour.nix;
        ghc-boot-th.revision = hackage.ghc-boot-th."9.6.6".revisions.default;
        pretty.revision = hackage.pretty."1.1.3.6".revisions.default;
        template-haskell.revision = hackage.template-haskell."2.20.0.0".revisions.default;
        deepseq.revision = hackage.deepseq."1.4.8.1".revisions.default;
        containers.revision = hackage.containers."0.6.7".revisions.default;
        array.revision = hackage.array."0.5.6.0".revisions.default;
        ghc-bignum.revision = hackage.ghc-bignum."1.3".revisions.default;
      };
      compiler = {
        version = "9.6.6";
        nix-name = "ghc966";
        packages = {
          "containers" = "0.6.7";
          "ghc-prim" = "0.10.0";
          "ghc-boot-th" = "9.6.6";
          "base" = "4.18.2.1";
          "ghc-bignum" = "1.3";
          "template-haskell" = "2.20.0.0";
          "pretty" = "1.1.3.6";
          "deepseq" = "1.4.8.1";
          "array" = "0.5.6.0";
        };
      };
    };
  extras = hackage:
    { packages = {}; };
  modules = [
    {
      preExistingPkgs = [
        "ghc-prim"
        "base"
        "ghc-boot-th"
        "pretty"
        "template-haskell"
        "deepseq"
        "containers"
        "array"
        "ghc-bignum"
      ];
    }
    ({ lib, ... }:
      { packages = {}; })
    ({ lib, ... }:
      {
        packages = {
          "hscolour".components.library.planned = lib.mkOverride 900 true;
          "deepseq".components.library.planned = lib.mkOverride 900 true;
          "base".components.library.planned = lib.mkOverride 900 true;
          "hscolour".components.exes."HsColour".planned = lib.mkOverride 900 true;
          "containers".components.library.planned = lib.mkOverride 900 true;
          "ghc-prim".components.library.planned = lib.mkOverride 900 true;
          "pretty".components.library.planned = lib.mkOverride 900 true;
          "template-haskell".components.library.planned = lib.mkOverride 900 true;
          "ghc-bignum".components.library.planned = lib.mkOverride 900 true;
          "array".components.library.planned = lib.mkOverride 900 true;
          "ghc-boot-th".components.library.planned = lib.mkOverride 900 true;
        };
      })
  ];
}