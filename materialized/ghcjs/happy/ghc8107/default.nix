{
  pkgs = hackage:
    {
      packages = {
        ghc-prim.revision = hackage.ghc-prim."0.6.1".revisions.default;
        transformers.revision = hackage.transformers."0.5.6.2".revisions.default;
        base.revision = hackage.base."4.14.3.0".revisions.default;
        mtl.revision = hackage.mtl."2.2.2".revisions.default;
        deepseq.revision = hackage.deepseq."1.4.4.0".revisions.default;
        integer-gmp.revision = hackage.integer-gmp."1.0.3.0".revisions.default;
        containers.revision = hackage.containers."0.6.5.1".revisions.default;
        array.revision = hackage.array."0.5.4.0".revisions.default;
        happy.revision = import ./cabal-files/happy.nix;
        happy.flags.small_base = true;
      };
      compiler = {
        version = "8.10.7";
        nix-name = "ghc8107";
        packages = {
          "transformers" = "0.5.6.2";
          "containers" = "0.6.5.1";
          "ghc-prim" = "0.6.1";
          "mtl" = "2.2.2";
          "base" = "4.14.3.0";
          "integer-gmp" = "1.0.3.0";
          "deepseq" = "1.4.4.0";
          "array" = "0.5.4.0";
        };
      };
    };
  extras = hackage:
    { packages = {}; };
  modules = [
    {
      preExistingPkgs = [
        "ghc-prim"
        "transformers"
        "base"
        "mtl"
        "deepseq"
        "integer-gmp"
        "containers"
        "array"
      ];
    }
    ({ lib, ... }:
      { packages = {}; })
    ({ lib, ... }:
      {
        packages = {
          "deepseq".components.library.planned = lib.mkOverride 900 true;
          "base".components.library.planned = lib.mkOverride 900 true;
          "integer-gmp".components.library.planned = lib.mkOverride 900 true;
          "transformers".components.library.planned = lib.mkOverride 900 true;
          "mtl".components.library.planned = lib.mkOverride 900 true;
          "containers".components.library.planned = lib.mkOverride 900 true;
          "ghc-prim".components.library.planned = lib.mkOverride 900 true;
          "happy".components.exes."happy".planned = lib.mkOverride 900 true;
          "array".components.library.planned = lib.mkOverride 900 true;
        };
      })
  ];
}