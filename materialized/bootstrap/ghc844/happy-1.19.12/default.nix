{
  pkgs = hackage:
    {
      packages = {
        "array".revision = (((hackage."array")."0.5.2.0").revisions).default;
        "mtl".revision = (((hackage."mtl")."2.2.2").revisions).default;
        "ghc-prim".revision = (((hackage."ghc-prim")."0.5.2.0").revisions).default;
        "base".revision = (((hackage."base")."4.11.1.0").revisions).default;
        "rts".revision = (((hackage."rts")."1.0").revisions).default;
        "transformers".revision = (((hackage."transformers")."0.5.5.0").revisions).default;
        "deepseq".revision = (((hackage."deepseq")."1.4.3.0").revisions).default;
        "integer-gmp".revision = (((hackage."integer-gmp")."1.0.2.0").revisions).default;
        "containers".revision = (((hackage."containers")."0.5.11.0").revisions).default;
        };
      compiler = {
        version = "8.4.4";
        nix-name = "ghc844";
        packages = {
          "array" = "0.5.2.0";
          "mtl" = "2.2.2";
          "ghc-prim" = "0.5.2.0";
          "base" = "4.11.1.0";
          "rts" = "1.0";
          "transformers" = "0.5.5.0";
          "deepseq" = "1.4.3.0";
          "integer-gmp" = "1.0.2.0";
          "containers" = "0.5.11.0";
          };
        };
      };
  extras = hackage:
    { packages = { happy = ./.plan.nix/happy.nix; }; };
  modules = [
    ({ lib, ... }:
      {
        packages = {
          "happy" = { flags = { "small_base" = lib.mkOverride 900 true; }; };
          };
        })
    ({ lib, ... }:
      {
        packages = {
          "ghc-prim".components.library.planned = lib.mkOverride 900 true;
          "array".components.library.planned = lib.mkOverride 900 true;
          "rts".components.library.planned = lib.mkOverride 900 true;
          "happy".components.exes."happy".planned = lib.mkOverride 900 true;
          "mtl".components.library.planned = lib.mkOverride 900 true;
          "transformers".components.library.planned = lib.mkOverride 900 true;
          "deepseq".components.library.planned = lib.mkOverride 900 true;
          "base".components.library.planned = lib.mkOverride 900 true;
          "integer-gmp".components.library.planned = lib.mkOverride 900 true;
          "containers".components.library.planned = lib.mkOverride 900 true;
          };
        })
    ];
  }