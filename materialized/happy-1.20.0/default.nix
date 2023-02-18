{
  pkgs = hackage:
    {
      packages = {
        mtl.revision = (((hackage.mtl)."2.2.2").revisions).default;
        ghc-bignum.revision = (((hackage.ghc-bignum)."1.1").revisions).default;
        ghc-prim.revision = (((hackage.ghc-prim)."0.7.0").revisions).default;
        containers.revision = (((hackage.containers)."0.6.4.1").revisions).default;
        base.revision = (((hackage.base)."4.15.1.0").revisions).default;
        deepseq.revision = (((hackage.deepseq)."1.4.5.0").revisions).default;
        rts.revision = (((hackage.rts)."1.0.2").revisions).default;
        transformers.revision = (((hackage.transformers)."0.5.6.2").revisions).default;
        array.revision = (((hackage.array)."0.5.4.0").revisions).default;
        };
      compiler = {
        version = "9.0.2";
        nix-name = "ghc902";
        packages = {
          "array" = "0.5.4.0";
          "mtl" = "2.2.2";
          "ghc-prim" = "0.7.0";
          "base" = "4.15.1.0";
          "ghc-bignum" = "1.1";
          "rts" = "1.0.2";
          "transformers" = "0.5.6.2";
          "deepseq" = "1.4.5.0";
          "containers" = "0.6.4.1";
          };
        };
      };
  extras = hackage:
    { packages = { happy = ./.plan.nix/happy.nix; }; };
  modules = [
    ({ lib, ... }:
      { packages = { "happy" = { flags = {}; }; }; })
    ({ lib, ... }:
      {
        packages = {
          "ghc-prim".components.library.planned = lib.mkOverride 900 true;
          "array".components.library.planned = lib.mkOverride 900 true;
          "rts".components.library.planned = lib.mkOverride 900 true;
          "happy".components.exes."happy".planned = lib.mkOverride 900 true;
          "ghc-bignum".components.library.planned = lib.mkOverride 900 true;
          "mtl".components.library.planned = lib.mkOverride 900 true;
          "transformers".components.library.planned = lib.mkOverride 900 true;
          "deepseq".components.library.planned = lib.mkOverride 900 true;
          "base".components.library.planned = lib.mkOverride 900 true;
          "containers".components.library.planned = lib.mkOverride 900 true;
          };
        })
    ];
  }