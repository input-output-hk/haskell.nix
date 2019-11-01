{
  pkgs = hackage:
    {
      packages = {
        "ghc-prim".revision = (((hackage."ghc-prim")."0.5.3").revisions).default;
        "stm".revision = (((hackage."stm")."2.5.0.0").revisions).default;
        "rts".revision = (((hackage."rts")."1.0").revisions).default;
        "base".revision = (((hackage."base")."4.12.0.0").revisions).default;
        "array".revision = (((hackage."array")."0.5.3.0").revisions).default;
        "integer-gmp".revision = (((hackage."integer-gmp")."1.0.2.0").revisions).default;
        };
      compiler = {
        version = "8.6.5";
        nix-name = "ghc865";
        packages = {
          "ghc-prim" = "0.5.3";
          "stm" = "2.5.0.0";
          "rts" = "1.0";
          "base" = "4.12.0.0";
          "array" = "0.5.3.0";
          "integer-gmp" = "1.0.2.0";
          };
        };
      };
  extras = hackage:
    { packages = { test-haddock = ./.plan.nix/test-haddock.nix; }; };
  modules = [
    ({ lib, ... }:
      { packages = { "test-haddock" = { flags = {}; }; }; })
    ];
  }