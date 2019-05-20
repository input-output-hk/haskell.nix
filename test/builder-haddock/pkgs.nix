{
  pkgs = hackage:
    {
      packages = {
        "ghc-prim".revision = (((hackage."ghc-prim")."0.5.2.0").revisions).default;
        "stm".revision = (((hackage."stm")."2.4.5.1").revisions).default;
        "rts".revision = (((hackage."rts")."1.0").revisions).default;
        "base".revision = (((hackage."base")."4.11.1.0").revisions).default;
        "array".revision = (((hackage."array")."0.5.2.0").revisions).default;
        "integer-gmp".revision = (((hackage."integer-gmp")."1.0.2.0").revisions).default;
        };
      compiler = {
        version = "8.4.4";
        nix-name = "ghc844";
        packages = {
          "ghc-prim" = "0.5.2.0";
          "stm" = "2.4.5.1";
          "rts" = "1.0";
          "base" = "4.11.1.0";
          "array" = "0.5.2.0";
          "integer-gmp" = "1.0.2.0";
          };
        };
      };
  extras = hackage:
    { packages = { test-haddock = ./.plan.nix/test-haddock.nix; }; };
  }