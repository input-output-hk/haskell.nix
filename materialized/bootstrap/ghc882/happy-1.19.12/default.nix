{
  pkgs = hackage:
    {
      packages = {
        "ghc-prim".revision = (((hackage."ghc-prim")."0.5.3").revisions).default;
        "array".revision = (((hackage."array")."0.5.4.0").revisions).default;
        "integer-gmp".revision = (((hackage."integer-gmp")."1.0.2.0").revisions).default;
        "process".revision = (((hackage."process")."1.6.7.0").revisions).default;
        "base".revision = (((hackage."base")."4.13.0.0").revisions).default;
        "rts".revision = (((hackage."rts")."1.0").revisions).default;
        "mtl".revision = (((hackage."mtl")."2.2.2").revisions).default;
        "time".revision = (((hackage."time")."1.9.3").revisions).default;
        "unix".revision = (((hackage."unix")."2.7.2.2").revisions).default;
        "bytestring".revision = (((hackage."bytestring")."0.10.10.0").revisions).default;
        "containers".revision = (((hackage."containers")."0.6.2.1").revisions).default;
        "directory".revision = (((hackage."directory")."1.3.4.0").revisions).default;
        "filepath".revision = (((hackage."filepath")."1.4.2.1").revisions).default;
        "deepseq".revision = (((hackage."deepseq")."1.4.4.0").revisions).default;
        "transformers".revision = (((hackage."transformers")."0.5.6.2").revisions).default;
        };
      compiler = {
        version = "8.8.2";
        nix-name = "ghc882";
        packages = {
          "ghc-prim" = "0.5.3";
          "array" = "0.5.4.0";
          "integer-gmp" = "1.0.2.0";
          "process" = "1.6.7.0";
          "base" = "4.13.0.0";
          "rts" = "1.0";
          "mtl" = "2.2.2";
          "time" = "1.9.3";
          "unix" = "2.7.2.2";
          "bytestring" = "0.10.10.0";
          "containers" = "0.6.2.1";
          "directory" = "1.3.4.0";
          "filepath" = "1.4.2.1";
          "deepseq" = "1.4.4.0";
          "transformers" = "0.5.6.2";
          };
        };
      };
  extras = hackage:
    { packages = { happy = ./.plan.nix/happy.nix; }; };
  modules = [
    ({ lib, ... }:
      {
        packages = {
          "happy" = { flags = { "small_base" = lib.mkOverride 900 true; }; };
          };
        })
    ({ lib, ... }:
      {
        packages = {
          "containers".components.library.planned = lib.mkOverride 900 true;
          "ghc-prim".components.library.planned = lib.mkOverride 900 true;
          "happy".components.exes."happy".planned = lib.mkOverride 900 true;
          "transformers".components.library.planned = lib.mkOverride 900 true;
          "process".components.library.planned = lib.mkOverride 900 true;
          "integer-gmp".components.library.planned = lib.mkOverride 900 true;
          "array".components.library.planned = lib.mkOverride 900 true;
          "bytestring".components.library.planned = lib.mkOverride 900 true;
          "unix".components.library.planned = lib.mkOverride 900 true;
          "base".components.library.planned = lib.mkOverride 900 true;
          "rts".components.library.planned = lib.mkOverride 900 true;
          "mtl".components.library.planned = lib.mkOverride 900 true;
          "time".components.library.planned = lib.mkOverride 900 true;
          "deepseq".components.library.planned = lib.mkOverride 900 true;
          "filepath".components.library.planned = lib.mkOverride 900 true;
          "happy".components.tests."tests".planned = lib.mkOverride 900 true;
          "directory".components.library.planned = lib.mkOverride 900 true;
          };
        })
    ];
  }