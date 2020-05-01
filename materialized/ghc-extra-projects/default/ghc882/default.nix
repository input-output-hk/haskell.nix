{
  pkgs = hackage:
    {
      packages = {
        "binary".revision = (((hackage."binary")."0.8.8.0").revisions).default;
        "ghc-prim".revision = (((hackage."ghc-prim")."0.5.3").revisions).default;
        "unix".revision = (((hackage."unix")."2.7.2.2").revisions).default;
        "ghc-heap".revision = (((hackage."ghc-heap")."8.8.2").revisions).default;
        "mtl".revision = (((hackage."mtl")."2.2.2").revisions).default;
        "rts".revision = (((hackage."rts")."1.0").revisions).default;
        "alex".revision = (((hackage."alex")."3.2.5").revisions).default;
        "alex".flags.small_base = true;
        "deepseq".revision = (((hackage."deepseq")."1.4.4.0").revisions).default;
        "network".revision = (((hackage."network")."2.8.0.1").revisions).default;
        "directory".revision = (((hackage."directory")."1.3.6.1").revisions).default;
        "template-haskell".revision = (((hackage."template-haskell")."2.15.0.0").revisions).default;
        "containers".revision = (((hackage."containers")."0.6.2.1").revisions).default;
        "base".revision = (((hackage."base")."4.13.0.0").revisions).default;
        "time".revision = (((hackage."time")."1.9.3").revisions).default;
        "terminfo".revision = (((hackage."terminfo")."0.4.1.4").revisions).default;
        "transformers".revision = (((hackage."transformers")."0.5.6.2").revisions).default;
        "happy".revision = (((hackage."happy")."1.19.12").revisions).default;
        "happy".flags.small_base = true;
        "hpc".revision = (((hackage."hpc")."0.6.0.3").revisions).default;
        "filepath".revision = (((hackage."filepath")."1.4.2.1").revisions).default;
        "process".revision = (((hackage."process")."1.6.8.2").revisions).default;
        "pretty".revision = (((hackage."pretty")."1.1.3.6").revisions).default;
        "ghc-boot-th".revision = (((hackage."ghc-boot-th")."8.8.2").revisions).default;
        "array".revision = (((hackage."array")."0.5.4.0").revisions).default;
        "integer-gmp".revision = (((hackage."integer-gmp")."1.0.2.0").revisions).default;
        };
      compiler = {
        version = "8.8.2";
        nix-name = "ghc882";
        packages = {
          "ghc-prim" = "0.5.3";
          "ghc-heap" = "8.8.2";
          "mtl" = "2.2.2";
          "rts" = "1.0";
          "deepseq" = "1.4.4.0";
          "template-haskell" = "2.15.0.0";
          "containers" = "0.6.2.1";
          "base" = "4.13.0.0";
          "time" = "1.9.3";
          "terminfo" = "0.4.1.4";
          "transformers" = "0.5.6.2";
          "filepath" = "1.4.2.1";
          "pretty" = "1.1.3.6";
          "ghc-boot-th" = "8.8.2";
          "array" = "0.5.4.0";
          "integer-gmp" = "1.0.2.0";
          };
        };
      };
  extras = hackage:
    {
      packages = {
        ghc = ./.plan.nix/ghc.nix;
        bytestring = ./.plan.nix/bytestring.nix;
        remote-iserv = ./.plan.nix/remote-iserv.nix;
        iserv-proxy = ./.plan.nix/iserv-proxy.nix;
        libiserv = ./.plan.nix/libiserv.nix;
        ghc-boot = ./.plan.nix/ghc-boot.nix;
        ghci = ./.plan.nix/ghci.nix;
        iserv = ./.plan.nix/iserv.nix;
        };
      };
  modules = [
    ({ lib, ... }:
      {
        packages = {
          "ghc" = {
            flags = {
              "stage1" = lib.mkOverride 900 false;
              "stage2" = lib.mkOverride 900 true;
              "integer-gmp" = lib.mkOverride 900 false;
              "stage3" = lib.mkOverride 900 false;
              "ghci" = lib.mkOverride 900 true;
              "integer-simple" = lib.mkOverride 900 false;
              "terminfo" = lib.mkOverride 900 true;
              };
            };
          "bytestring" = {
            flags = { "integer-simple" = lib.mkOverride 900 false; };
            };
          "remote-iserv" = { flags = {}; };
          "iserv-proxy" = { flags = {}; };
          "libiserv" = { flags = { "network" = lib.mkOverride 900 true; }; };
          "ghc-boot" = { flags = {}; };
          "ghci" = { flags = { "ghci" = lib.mkOverride 900 true; }; };
          "iserv" = { flags = {}; };
          };
        })
    ];
  }