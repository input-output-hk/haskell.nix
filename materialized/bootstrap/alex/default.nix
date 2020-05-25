{
  pkgs = hackage:
    {
      packages = {
        "ghc-prim".revision = (((hackage."ghc-prim")."0.5.2.0").revisions).default;
        "unix".revision = (((hackage."unix")."2.7.2.2").revisions).default;
        "rts".revision = (((hackage."rts")."1.0").revisions).default;
        "deepseq".revision = (((hackage."deepseq")."1.4.3.0").revisions).default;
        "directory".revision = (((hackage."directory")."1.3.1.5").revisions).default;
        "containers".revision = (((hackage."containers")."0.5.11.0").revisions).default;
        "bytestring".revision = (((hackage."bytestring")."0.10.8.2").revisions).default;
        "base".revision = (((hackage."base")."4.11.1.0").revisions).default;
        "time".revision = (((hackage."time")."1.8.0.2").revisions).default;
        "filepath".revision = (((hackage."filepath")."1.4.2").revisions).default;
        "process".revision = (((hackage."process")."1.6.3.0").revisions).default;
        "array".revision = (((hackage."array")."0.5.2.0").revisions).default;
        "integer-gmp".revision = (((hackage."integer-gmp")."1.0.2.0").revisions).default;
        };
      compiler = {
        version = "8.4.4";
        nix-name = "ghc844";
        packages = {
          "ghc-prim" = "0.5.2.0";
          "unix" = "2.7.2.2";
          "rts" = "1.0";
          "deepseq" = "1.4.3.0";
          "directory" = "1.3.1.5";
          "containers" = "0.5.11.0";
          "bytestring" = "0.10.8.2";
          "base" = "4.11.1.0";
          "time" = "1.8.0.2";
          "filepath" = "1.4.2";
          "process" = "1.6.3.0";
          "array" = "0.5.2.0";
          "integer-gmp" = "1.0.2.0";
          };
        };
      };
  extras = hackage:
    { packages = { alex = ./.plan.nix/alex.nix; }; };
  modules = [
    ({ lib, ... }:
      {
        packages = {
          "alex" = { flags = { "small_base" = lib.mkOverride 900 true; }; };
          };
        })
    ];
  }