{
  pkgs = hackage:
    {
      packages = {
        "ghc-boot-th".revision = (((hackage."ghc-boot-th")."9.0.1").revisions).default;
        "ghc-prim".revision = (((hackage."ghc-prim")."0.7.0").revisions).default;
        "pretty".revision = (((hackage."pretty")."1.1.3.6").revisions).default;
        "base".revision = (((hackage."base")."4.15.0.0").revisions).default;
        "time".revision = (((hackage."time")."1.9.3").revisions).default;
        "array".revision = (((hackage."array")."0.5.4.0").revisions).default;
        "process".revision = (((hackage."process")."1.6.11.0").revisions).default;
        "network".revision = (((hackage."network")."2.8.0.1").revisions).default;
        "ghc-bignum".revision = (((hackage."ghc-bignum")."1.0").revisions).default;
        "exceptions".revision = (((hackage."exceptions")."0.10.4").revisions).default;
        "directory".revision = (((hackage."directory")."1.3.6.1").revisions).default;
        "mtl".revision = (((hackage."mtl")."2.2.2").revisions).default;
        "transformers".revision = (((hackage."transformers")."0.5.6.2").revisions).default;
        "rts".revision = (((hackage."rts")."1.0").revisions).default;
        "template-haskell".revision = (((hackage."template-haskell")."2.17.0.0").revisions).default;
        "bytestring".revision = (((hackage."bytestring")."0.10.12.1").revisions).default;
        "deepseq".revision = (((hackage."deepseq")."1.4.5.0").revisions).default;
        "unix".revision = (((hackage."unix")."2.7.2.2").revisions).default;
        "filepath".revision = (((hackage."filepath")."1.4.2.1").revisions).default;
        "stm".revision = (((hackage."stm")."2.5.0.0").revisions).default;
        "terminfo".revision = (((hackage."terminfo")."0.4.1.5").revisions).default;
        "ghc-heap".revision = (((hackage."ghc-heap")."9.0.1").revisions).default;
        "binary".revision = (((hackage."binary")."0.8.8.0").revisions).default;
        "containers".revision = (((hackage."containers")."0.6.4.1").revisions).default;
        };
      compiler = {
        version = "9.0.1";
        nix-name = "ghc901";
        packages = {
          "ghc-boot-th" = "9.0.1";
          "ghc-prim" = "0.7.0";
          "pretty" = "1.1.3.6";
          "base" = "4.15.0.0";
          "time" = "1.9.3";
          "array" = "0.5.4.0";
          "process" = "1.6.11.0";
          "ghc-bignum" = "1.0";
          "exceptions" = "0.10.4";
          "directory" = "1.3.6.1";
          "mtl" = "2.2.2";
          "transformers" = "0.5.6.2";
          "rts" = "1.0";
          "template-haskell" = "2.17.0.0";
          "bytestring" = "0.10.12.1";
          "deepseq" = "1.4.5.0";
          "unix" = "2.7.2.2";
          "filepath" = "1.4.2.1";
          "stm" = "2.5.0.0";
          "ghc-heap" = "9.0.1";
          "binary" = "0.8.8.0";
          "containers" = "0.6.4.1";
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
              "dynamic-system-linker" = lib.mkOverride 900 true;
              "stage3" = lib.mkOverride 900 false;
              "ghci" = lib.mkOverride 900 true;
              "stage2" = lib.mkOverride 900 false;
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
          "ghc-bignum".components.library.planned = lib.mkOverride 900 true;
          "filepath".components.library.planned = lib.mkOverride 900 true;
          "pretty".components.library.planned = lib.mkOverride 900 true;
          "process".components.library.planned = lib.mkOverride 900 true;
          "bytestring".components.library.planned = lib.mkOverride 900 true;
          "template-haskell".components.library.planned = lib.mkOverride 900 true;
          "stm".components.library.planned = lib.mkOverride 900 true;
          "ghci".components.library.planned = lib.mkOverride 900 true;
          "remote-iserv".components.exes."remote-iserv".planned = lib.mkOverride 900 true;
          "exceptions".components.library.planned = lib.mkOverride 900 true;
          "ghc-prim".components.library.planned = lib.mkOverride 900 true;
          "ghc-boot".components.library.planned = lib.mkOverride 900 true;
          "hpc".components.library.planned = lib.mkOverride 900 true;
          "array".components.library.planned = lib.mkOverride 900 true;
          "binary".components.library.planned = lib.mkOverride 900 true;
          "ghc-heap".components.library.planned = lib.mkOverride 900 true;
          "ghc-boot-th".components.library.planned = lib.mkOverride 900 true;
          "mtl".components.library.planned = lib.mkOverride 900 true;
          "rts".components.library.planned = lib.mkOverride 900 true;
          "unix".components.library.planned = lib.mkOverride 900 true;
          "transformers".components.library.planned = lib.mkOverride 900 true;
          "deepseq".components.library.planned = lib.mkOverride 900 true;
          "libiserv".components.library.planned = lib.mkOverride 900 true;
          "directory".components.library.planned = lib.mkOverride 900 true;
          "iserv".components.exes."iserv".planned = lib.mkOverride 900 true;
          "base".components.library.planned = lib.mkOverride 900 true;
          "network".components.library.planned = lib.mkOverride 900 true;
          "time".components.library.planned = lib.mkOverride 900 true;
          "containers".components.library.planned = lib.mkOverride 900 true;
          "ghc".components.library.planned = lib.mkOverride 900 true;
          "iserv-proxy".components.exes."iserv-proxy".planned = lib.mkOverride 900 true;
          "terminfo".components.library.planned = lib.mkOverride 900 true;
          };
        })
    ];
  }