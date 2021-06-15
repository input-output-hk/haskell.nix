{
  pkgs = hackage:
    {
      packages = {
        "binary".revision = (((hackage."binary")."0.8.8.0").revisions).default;
        "ghc-prim".revision = (((hackage."ghc-prim")."0.6.1").revisions).default;
        "ghc-heap".revision = (((hackage."ghc-heap")."8.10.5").revisions).default;
        "rts".revision = (((hackage."rts")."1.0.1").revisions).default;
        "deepseq".revision = (((hackage."deepseq")."1.4.4.0").revisions).default;
        "network".revision = (((hackage."network")."2.8.0.1").revisions).default;
        "directory".revision = (((hackage."directory")."1.3.6.1").revisions).default;
        "template-haskell".revision = (((hackage."template-haskell")."2.16.0.0").revisions).default;
        "containers".revision = (((hackage."containers")."0.6.4.1").revisions).default;
        "base".revision = (((hackage."base")."4.14.2.0").revisions).default;
        "time".revision = (((hackage."time")."1.9.3").revisions).default;
        "transformers".revision = (((hackage."transformers")."0.5.6.2").revisions).default;
        "filepath".revision = (((hackage."filepath")."1.4.2.1").revisions).default;
        "process".revision = (((hackage."process")."1.6.11.0").revisions).default;
        "pretty".revision = (((hackage."pretty")."1.1.3.6").revisions).default;
        "ghc-boot-th".revision = (((hackage."ghc-boot-th")."8.10.5").revisions).default;
        "array".revision = (((hackage."array")."0.5.4.0").revisions).default;
        "Win32".revision = (((hackage."Win32")."2.6.2.0").revisions).default;
        "integer-gmp".revision = (((hackage."integer-gmp")."1.0.3.0").revisions).default;
        };
      compiler = {
        version = "8.10.5";
        nix-name = "ghc8105";
        packages = {
          "ghc-prim" = "0.6.1";
          "ghc-heap" = "8.10.5";
          "rts" = "1.0.1";
          "deepseq" = "1.4.4.0";
          "template-haskell" = "2.16.0.0";
          "containers" = "0.6.4.1";
          "base" = "4.14.2.0";
          "transformers" = "0.5.6.2";
          "filepath" = "1.4.2.1";
          "pretty" = "1.1.3.6";
          "ghc-boot-th" = "8.10.5";
          "array" = "0.5.4.0";
          "integer-gmp" = "1.0.3.0";
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
        hpc = ./.plan.nix/hpc.nix;
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
              "stage2" = lib.mkOverride 900 false;
              "integer-gmp" = lib.mkOverride 900 false;
              "stage3" = lib.mkOverride 900 false;
              "dynamic-system-linker" = lib.mkOverride 900 true;
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
          "hpc" = { flags = {}; };
          "libiserv" = { flags = { "network" = lib.mkOverride 900 true; }; };
          "ghc-boot" = { flags = {}; };
          "ghci" = { flags = { "ghci" = lib.mkOverride 900 true; }; };
          "iserv" = { flags = {}; };
          };
        })
    ];
  }