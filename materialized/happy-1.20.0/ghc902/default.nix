{
  pkgs = hackage:
    {
      packages = {
        ghc-prim.revision = hackage.ghc-prim."0.7.0".revisions.default;
        transformers.revision = hackage.transformers."0.5.6.2".revisions.default;
        base.revision = hackage.base."4.15.1.0".revisions.default;
        mtl.revision = hackage.mtl."2.2.2".revisions.default;
        deepseq.revision = hackage.deepseq."1.4.5.0".revisions.default;
        containers.revision = hackage.containers."0.6.4.1".revisions.default;
        array.revision = hackage.array."0.5.4.0".revisions.default;
        ghc-bignum.revision = hackage.ghc-bignum."1.1".revisions.default;
      };
      compiler = {
        version = "9.0.2";
        nix-name = "ghc902";
        packages = {
          "transformers" = "0.5.6.2";
          "containers" = "0.6.4.1";
          "ghc-prim" = "0.7.0";
          "mtl" = "2.2.2";
          "base" = "4.15.1.0";
          "ghc-bignum" = "1.1";
          "deepseq" = "1.4.5.0";
          "array" = "0.5.4.0";
        };
      };
    };
  extras = hackage:
    { packages = { happy = ./.plan.nix/happy.nix; }; };
  modules = [
    {
      preExistingPkgs = [
        "ghc-prim"
        "transformers"
        "base"
        "mtl"
        "deepseq"
        "containers"
        "array"
        "ghc-bignum"
      ];
    }
    ({ lib, ... }:
      { packages = { "happy" = { flags = {}; }; }; })
    ({ lib, ... }:
      {
        packages = {
          "deepseq".components.library.planned = lib.mkOverride 900 true;
          "base".components.library.planned = lib.mkOverride 900 true;
          "transformers".components.library.planned = lib.mkOverride 900 true;
          "mtl".components.library.planned = lib.mkOverride 900 true;
          "containers".components.library.planned = lib.mkOverride 900 true;
          "ghc-prim".components.library.planned = lib.mkOverride 900 true;
          "happy".components.exes."happy".planned = lib.mkOverride 900 true;
          "ghc-bignum".components.library.planned = lib.mkOverride 900 true;
          "array".components.library.planned = lib.mkOverride 900 true;
        };
      })
  ];
}