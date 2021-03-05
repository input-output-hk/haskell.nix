{
  pkgs = hackage:
    {
      packages = {
        "ghc-prim".revision = (((hackage."ghc-prim")."0.5.3").revisions).default;
        "mtl".revision = (((hackage."mtl")."2.2.2").revisions).default;
        "rts".revision = (((hackage."rts")."1.0").revisions).default;
        "deepseq".revision = (((hackage."deepseq")."1.4.4.0").revisions).default;
        "containers".revision = (((hackage."containers")."0.6.0.1").revisions).default;
        "base".revision = (((hackage."base")."4.12.0.0").revisions).default;
        "transformers".revision = (((hackage."transformers")."0.5.6.2").revisions).default;
        "array".revision = (((hackage."array")."0.5.3.0").revisions).default;
        "integer-gmp".revision = (((hackage."integer-gmp")."1.0.2.0").revisions).default;
        };
      compiler = {
        version = "8.6.5";
        nix-name = "ghc865";
        packages = {
          "ghc-prim" = "0.5.3";
          "mtl" = "2.2.2";
          "rts" = "1.0";
          "deepseq" = "1.4.4.0";
          "containers" = "0.6.0.1";
          "base" = "4.12.0.0";
          "transformers" = "0.5.6.2";
          "array" = "0.5.3.0";
          "integer-gmp" = "1.0.2.0";
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
    ];
  }