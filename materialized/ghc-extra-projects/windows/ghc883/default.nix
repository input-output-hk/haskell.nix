{
  pkgs = hackage:
    {
      packages = {
        "happy".revision = (((hackage."happy")."1.20.0").revisions).default;
        "ghc-boot-th".revision = (((hackage."ghc-boot-th")."8.8.3").revisions).default;
        "ghc-prim".revision = (((hackage."ghc-prim")."0.5.3").revisions).default;
        "pretty".revision = (((hackage."pretty")."1.1.3.6").revisions).default;
        "base".revision = (((hackage."base")."4.13.0.0").revisions).default;
        "time".revision = (((hackage."time")."1.9.3").revisions).default;
        "array".revision = (((hackage."array")."0.5.4.0").revisions).default;
        "process".revision = (((hackage."process")."1.6.13.2").revisions).default;
        "network".revision = (((hackage."network")."2.8.0.1").revisions).default;
        "directory".revision = (((hackage."directory")."1.3.7.0").revisions).default;
        "alex".revision = (((hackage."alex")."3.2.6").revisions).default;
        "alex".flags.small_base = true;
        "mtl".revision = (((hackage."mtl")."2.2.2").revisions).default;
        "transformers".revision = (((hackage."transformers")."0.5.6.2").revisions).default;
        "rts".revision = (((hackage."rts")."1.0").revisions).default;
        "template-haskell".revision = (((hackage."template-haskell")."2.15.0.0").revisions).default;
        "bytestring".revision = (((hackage."bytestring")."0.10.10.0").revisions).default;
        "deepseq".revision = (((hackage."deepseq")."1.4.4.0").revisions).default;
        "filepath".revision = (((hackage."filepath")."1.4.2.1").revisions).default;
        "integer-gmp".revision = (((hackage."integer-gmp")."1.0.2.0").revisions).default;
        "ghc-heap".revision = (((hackage."ghc-heap")."8.8.3").revisions).default;
        "binary".revision = (((hackage."binary")."0.8.7.0").revisions).default;
        "containers".revision = (((hackage."containers")."0.6.2.1").revisions).default;
        };
      compiler = {
        version = "8.8.3";
        nix-name = "ghc883";
        packages = {
          "ghc-boot-th" = "8.8.3";
          "ghc-prim" = "0.5.3";
          "pretty" = "1.1.3.6";
          "base" = "4.13.0.0";
          "array" = "0.5.4.0";
          "mtl" = "2.2.2";
          "transformers" = "0.5.6.2";
          "rts" = "1.0";
          "template-haskell" = "2.15.0.0";
          "bytestring" = "0.10.10.0";
          "deepseq" = "1.4.4.0";
          "filepath" = "1.4.2.1";
          "integer-gmp" = "1.0.2.0";
          "ghc-heap" = "8.8.3";
          "binary" = "0.8.7.0";
          "containers" = "0.6.2.1";
          };
        };
      };
  extras = hackage:
    {
      packages = {
        remote-iserv = ./.plan.nix/remote-iserv.nix;
        iserv-proxy = ./.plan.nix/iserv-proxy.nix;
        ghci = ./.plan.nix/ghci.nix;
        ghc-boot = ./.plan.nix/ghc-boot.nix;
        iserv = ./.plan.nix/iserv.nix;
        ghc = ./.plan.nix/ghc.nix;
        libiserv = ./.plan.nix/libiserv.nix;
        Win32 = ./.plan.nix/Win32.nix;
        hpc = ./.plan.nix/hpc.nix;
        };
      };
  modules = [
    ({ lib, ... }:
      {
        packages = {
          "remote-iserv" = { flags = {}; };
          "iserv-proxy" = { flags = {}; };
          "ghci" = { flags = { "ghci" = lib.mkOverride 900 true; }; };
          "ghc-boot" = { flags = {}; };
          "iserv" = { flags = {}; };
          "ghc" = {
            flags = {
              "stage3" = lib.mkOverride 900 false;
              "ghci" = lib.mkOverride 900 true;
              "integer-gmp" = lib.mkOverride 900 false;
              "stage2" = lib.mkOverride 900 true;
              "integer-simple" = lib.mkOverride 900 false;
              "stage1" = lib.mkOverride 900 false;
              "terminfo" = lib.mkOverride 900 true;
              };
            };
          "libiserv" = { flags = { "network" = lib.mkOverride 900 true; }; };
          "Win32" = { flags = {}; };
          "hpc" = { flags = {}; };
          };
        })
    ({ lib, ... }:
      {
        packages = {
          "filepath".components.library.planned = lib.mkOverride 900 true;
          "pretty".components.library.planned = lib.mkOverride 900 true;
          "process".components.library.planned = lib.mkOverride 900 true;
          "bytestring".components.library.planned = lib.mkOverride 900 true;
          "template-haskell".components.library.planned = lib.mkOverride 900 true;
          "ghci".components.library.planned = lib.mkOverride 900 true;
          "remote-iserv".components.exes."remote-iserv".planned = lib.mkOverride 900 true;
          "alex".components.exes."alex".planned = lib.mkOverride 900 true;
          "ghc-prim".components.library.planned = lib.mkOverride 900 true;
          "ghc-boot".components.library.planned = lib.mkOverride 900 true;
          "hpc".components.library.planned = lib.mkOverride 900 true;
          "array".components.library.planned = lib.mkOverride 900 true;
          "Win32".components.library.planned = lib.mkOverride 900 true;
          "binary".components.library.planned = lib.mkOverride 900 true;
          "ghc-heap".components.library.planned = lib.mkOverride 900 true;
          "ghc-boot-th".components.library.planned = lib.mkOverride 900 true;
          "mtl".components.library.planned = lib.mkOverride 900 true;
          "rts".components.library.planned = lib.mkOverride 900 true;
          "transformers".components.library.planned = lib.mkOverride 900 true;
          "deepseq".components.library.planned = lib.mkOverride 900 true;
          "libiserv".components.library.planned = lib.mkOverride 900 true;
          "directory".components.library.planned = lib.mkOverride 900 true;
          "happy".components.exes."happy".planned = lib.mkOverride 900 true;
          "iserv".components.exes."iserv".planned = lib.mkOverride 900 true;
          "base".components.library.planned = lib.mkOverride 900 true;
          "network".components.library.planned = lib.mkOverride 900 true;
          "time".components.library.planned = lib.mkOverride 900 true;
          "integer-gmp".components.library.planned = lib.mkOverride 900 true;
          "containers".components.library.planned = lib.mkOverride 900 true;
          "ghc".components.library.planned = lib.mkOverride 900 true;
          "iserv-proxy".components.exes."iserv-proxy".planned = lib.mkOverride 900 true;
          };
        })
    ];
  }