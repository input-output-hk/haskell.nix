{
  pkgs = hackage:
    {
      packages = {
        "binary".revision = (((hackage."binary")."0.8.5.1").revisions).default;
        "ghc-prim".revision = (((hackage."ghc-prim")."0.5.2.0").revisions).default;
        "array".revision = (((hackage."array")."0.5.2.0").revisions).default;
        "integer-gmp".revision = (((hackage."integer-gmp")."1.0.2.0").revisions).default;
        "pretty".revision = (((hackage."pretty")."1.1.3.6").revisions).default;
        "process".revision = (((hackage."process")."1.6.3.0").revisions).default;
        "base".revision = (((hackage."base")."4.11.1.0").revisions).default;
        "rts".revision = (((hackage."rts")."1.0").revisions).default;
        "text".revision = (((hackage."text")."1.2.3.1").revisions).default;
        "mtl".revision = (((hackage."mtl")."2.2.2").revisions).default;
        "time".revision = (((hackage."time")."1.8.0.2").revisions).default;
        "unix".revision = (((hackage."unix")."2.7.2.2").revisions).default;
        "bytestring".revision = (((hackage."bytestring")."0.10.8.2").revisions).default;
        "containers".revision = (((hackage."containers")."0.5.11.0").revisions).default;
        "directory".revision = (((hackage."directory")."1.3.1.5").revisions).default;
        "parsec".revision = (((hackage."parsec")."3.1.13.0").revisions).default;
        "filepath".revision = (((hackage."filepath")."1.4.2").revisions).default;
        "deepseq".revision = (((hackage."deepseq")."1.4.3.0").revisions).default;
        "transformers".revision = (((hackage."transformers")."0.5.5.0").revisions).default;
        "Cabal".revision = (((hackage."Cabal")."2.2.0.1").revisions).default;
        };
      compiler = {
        version = "8.4.4";
        nix-name = "ghc844";
        packages = {
          "binary" = "0.8.5.1";
          "ghc-prim" = "0.5.2.0";
          "array" = "0.5.2.0";
          "integer-gmp" = "1.0.2.0";
          "pretty" = "1.1.3.6";
          "process" = "1.6.3.0";
          "base" = "4.11.1.0";
          "rts" = "1.0";
          "text" = "1.2.3.1";
          "mtl" = "2.2.2";
          "time" = "1.8.0.2";
          "unix" = "2.7.2.2";
          "bytestring" = "0.10.8.2";
          "containers" = "0.5.11.0";
          "directory" = "1.3.1.5";
          "parsec" = "3.1.13.0";
          "filepath" = "1.4.2";
          "deepseq" = "1.4.3.0";
          "transformers" = "0.5.5.0";
          "Cabal" = "2.2.0.1";
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
          "binary".components.library.planned = lib.mkOverride 900 true;
          "ghc-prim".components.library.planned = lib.mkOverride 900 true;
          "happy".components.exes."happy".planned = lib.mkOverride 900 true;
          "Cabal".components.library.planned = lib.mkOverride 900 true;
          "transformers".components.library.planned = lib.mkOverride 900 true;
          "process".components.library.planned = lib.mkOverride 900 true;
          "pretty".components.library.planned = lib.mkOverride 900 true;
          "integer-gmp".components.library.planned = lib.mkOverride 900 true;
          "array".components.library.planned = lib.mkOverride 900 true;
          "bytestring".components.library.planned = lib.mkOverride 900 true;
          "unix".components.library.planned = lib.mkOverride 900 true;
          "text".components.library.planned = lib.mkOverride 900 true;
          "base".components.library.planned = lib.mkOverride 900 true;
          "rts".components.library.planned = lib.mkOverride 900 true;
          "mtl".components.library.planned = lib.mkOverride 900 true;
          "time".components.library.planned = lib.mkOverride 900 true;
          "happy".components.setup.planned = lib.mkOverride 900 true;
          "deepseq".components.library.planned = lib.mkOverride 900 true;
          "filepath".components.library.planned = lib.mkOverride 900 true;
          "parsec".components.library.planned = lib.mkOverride 900 true;
          "directory".components.library.planned = lib.mkOverride 900 true;
          };
        })
    ];
  }