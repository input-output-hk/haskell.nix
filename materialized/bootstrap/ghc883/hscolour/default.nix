{
  pkgs = hackage:
    {
      packages = {
        "array".revision = (((hackage."array")."0.5.4.0").revisions).default;
        "ghc-prim".revision = (((hackage."ghc-prim")."0.5.3").revisions).default;
        "base".revision = (((hackage."base")."4.13.0.0").revisions).default;
        "rts".revision = (((hackage."rts")."1.0").revisions).default;
        "deepseq".revision = (((hackage."deepseq")."1.4.4.0").revisions).default;
        "integer-gmp".revision = (((hackage."integer-gmp")."1.0.2.0").revisions).default;
        "containers".revision = (((hackage."containers")."0.6.2.1").revisions).default;
        };
      compiler = {
        version = "8.8.3";
        nix-name = "ghc883";
        packages = {
          "array" = "0.5.4.0";
          "ghc-prim" = "0.5.3";
          "base" = "4.13.0.0";
          "rts" = "1.0";
          "deepseq" = "1.4.4.0";
          "integer-gmp" = "1.0.2.0";
          "containers" = "0.6.2.1";
          };
        };
      };
  extras = hackage:
    { packages = { hscolour = ./.plan.nix/hscolour.nix; }; };
  modules = [
    ({ lib, ... }:
      { packages = { "hscolour" = { flags = {}; }; }; })
    ({ lib, ... }:
      {
        packages = {
          "ghc-prim".components.library.planned = lib.mkOverride 900 true;
          "array".components.library.planned = lib.mkOverride 900 true;
          "rts".components.library.planned = lib.mkOverride 900 true;
          "hscolour".components.library.planned = lib.mkOverride 900 true;
          "hscolour".components.exes."HsColour".planned = lib.mkOverride 900 true;
          "deepseq".components.library.planned = lib.mkOverride 900 true;
          "base".components.library.planned = lib.mkOverride 900 true;
          "integer-gmp".components.library.planned = lib.mkOverride 900 true;
          "containers".components.library.planned = lib.mkOverride 900 true;
          };
        })
    ];
  }