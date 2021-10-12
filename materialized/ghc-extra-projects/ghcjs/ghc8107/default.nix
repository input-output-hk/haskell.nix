{
  pkgs = hackage:
    {
      packages = {
        "binary".revision = (((hackage."binary")."0.8.8.0").revisions).default;
        "ghc-prim".revision = (((hackage."ghc-prim")."0.6.1").revisions).default;
        "array".revision = (((hackage."array")."0.5.4.0").revisions).default;
        "integer-gmp".revision = (((hackage."integer-gmp")."1.0.3.0").revisions).default;
        "template-haskell".revision = (((hackage."template-haskell")."2.16.0.0").revisions).default;
        "network".revision = (((hackage."network")."2.8.0.1").revisions).default;
        "pretty".revision = (((hackage."pretty")."1.1.3.6").revisions).default;
        "process".revision = (((hackage."process")."1.6.11.0").revisions).default;
        "base".revision = (((hackage."base")."4.14.3.0").revisions).default;
        "rts".revision = (((hackage."rts")."1.0").revisions).default;
        "time".revision = (((hackage."time")."1.9.3").revisions).default;
        "unix".revision = (((hackage."unix")."2.7.2.2").revisions).default;
        "containers".revision = (((hackage."containers")."0.6.5.1").revisions).default;
        "directory".revision = (((hackage."directory")."1.3.6.2").revisions).default;
        "ghc-boot-th".revision = (((hackage."ghc-boot-th")."8.10.7").revisions).default;
        "filepath".revision = (((hackage."filepath")."1.4.2.1").revisions).default;
        "deepseq".revision = (((hackage."deepseq")."1.4.4.0").revisions).default;
        "transformers".revision = (((hackage."transformers")."0.5.6.2").revisions).default;
        "terminfo".revision = (((hackage."terminfo")."0.4.1.4").revisions).default;
        "ghc-heap".revision = (((hackage."ghc-heap")."8.10.7").revisions).default;
        };
      compiler = {
        version = "8.10.7";
        nix-name = "ghc8107";
        packages = {
          "ghc-prim" = "0.6.1";
          "array" = "0.5.4.0";
          "integer-gmp" = "1.0.3.0";
          "template-haskell" = "2.16.0.0";
          "pretty" = "1.1.3.6";
          "base" = "4.14.3.0";
          "rts" = "1.0";
          "time" = "1.9.3";
          "containers" = "0.6.5.1";
          "ghc-boot-th" = "8.10.7";
          "filepath" = "1.4.2.1";
          "deepseq" = "1.4.4.0";
          "transformers" = "0.5.6.2";
          "ghc-heap" = "8.10.7";
          };
        };
      };
  extras = hackage:
    {
      packages = {
        bytestring = ./.plan.nix/bytestring.nix;
        iserv = ./.plan.nix/iserv.nix;
        ghci = ./.plan.nix/ghci.nix;
        remote-iserv = ./.plan.nix/remote-iserv.nix;
        ghc-boot = ./.plan.nix/ghc-boot.nix;
        ghc = ./.plan.nix/ghc.nix;
        libiserv = ./.plan.nix/libiserv.nix;
        Win32 = ./.plan.nix/Win32.nix;
        hpc = ./.plan.nix/hpc.nix;
        iserv-proxy = ./.plan.nix/iserv-proxy.nix;
        };
      };
  modules = [
    ({ lib, ... }:
      {
        packages = {
          "bytestring" = {
            flags = { "integer-simple" = lib.mkOverride 900 false; };
            };
          "iserv" = { flags = {}; };
          "ghci" = { flags = { "ghci" = lib.mkOverride 900 true; }; };
          "remote-iserv" = { flags = {}; };
          "ghc-boot" = { flags = {}; };
          "ghc" = {
            flags = {
              "dynamic-system-linker" = lib.mkOverride 900 true;
              "ghci" = lib.mkOverride 900 true;
              "integer-simple" = lib.mkOverride 900 false;
              "stage3" = lib.mkOverride 900 false;
              "integer-gmp" = lib.mkOverride 900 false;
              "stage2" = lib.mkOverride 900 false;
              "terminfo" = lib.mkOverride 900 true;
              "stage1" = lib.mkOverride 900 false;
              };
            };
          "libiserv" = { flags = { "network" = lib.mkOverride 900 true; }; };
          "Win32" = { flags = {}; };
          "hpc" = { flags = {}; };
          "iserv-proxy" = { flags = {}; };
          };
        })
    ({ lib, ... }:
      {
        packages = {
          "libiserv".components.library.planned = lib.mkOverride 900 true;
          "ghc".components.library.planned = lib.mkOverride 900 true;
          "containers".components.library.planned = lib.mkOverride 900 true;
          "ghc-boot".components.library.planned = lib.mkOverride 900 true;
          "binary".components.library.planned = lib.mkOverride 900 true;
          "ghc-prim".components.library.planned = lib.mkOverride 900 true;
          "iserv".components.exes."iserv".planned = lib.mkOverride 900 true;
          "iserv-proxy".components.exes."iserv-proxy".planned = lib.mkOverride 900 true;
          "ghc-heap".components.library.planned = lib.mkOverride 900 true;
          "ghci".components.library.planned = lib.mkOverride 900 true;
          "terminfo".components.library.planned = lib.mkOverride 900 true;
          "transformers".components.library.planned = lib.mkOverride 900 true;
          "process".components.library.planned = lib.mkOverride 900 true;
          "hpc".components.library.planned = lib.mkOverride 900 true;
          "network".components.library.planned = lib.mkOverride 900 true;
          "pretty".components.library.planned = lib.mkOverride 900 true;
          "template-haskell".components.library.planned = lib.mkOverride 900 true;
          "integer-gmp".components.library.planned = lib.mkOverride 900 true;
          "array".components.library.planned = lib.mkOverride 900 true;
          "bytestring".components.library.planned = lib.mkOverride 900 true;
          "remote-iserv".components.exes."remote-iserv".planned = lib.mkOverride 900 true;
          "unix".components.library.planned = lib.mkOverride 900 true;
          "base".components.library.planned = lib.mkOverride 900 true;
          "rts".components.library.planned = lib.mkOverride 900 true;
          "time".components.library.planned = lib.mkOverride 900 true;
          "deepseq".components.library.planned = lib.mkOverride 900 true;
          "filepath".components.library.planned = lib.mkOverride 900 true;
          "ghc-boot-th".components.library.planned = lib.mkOverride 900 true;
          "directory".components.library.planned = lib.mkOverride 900 true;
          };
        })
    ];
  }