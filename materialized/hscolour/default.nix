{
  pkgs = hackage:
    {
      packages = {
        "ghc-prim".revision = (((hackage."ghc-prim")."0.5.2.0").revisions).default;
        "rts".revision = (((hackage."rts")."1.0").revisions).default;
        "deepseq".revision = (((hackage."deepseq")."1.4.3.0").revisions).default;
        "containers".revision = (((hackage."containers")."0.5.11.0").revisions).default;
        "base".revision = (((hackage."base")."4.11.1.0").revisions).default;
        "array".revision = (((hackage."array")."0.5.2.0").revisions).default;
        "integer-gmp".revision = (((hackage."integer-gmp")."1.0.2.0").revisions).default;
        };
      compiler = {
        version = "8.4.4";
        nix-name = "ghc844";
        packages = {
          "ghc-prim" = "0.5.2.0";
          "rts" = "1.0";
          "deepseq" = "1.4.3.0";
          "containers" = "0.5.11.0";
          "base" = "4.11.1.0";
          "array" = "0.5.2.0";
          "integer-gmp" = "1.0.2.0";
          };
        };
      };
  extras = hackage:
    { packages = { hscolour = ./.plan.nix/hscolour.nix; }; };
  modules = [
    ({ lib, ... }:
      { packages = { "hscolour" = { flags = {}; }; }; })
    ];
  }