{
  pkgs = hackage:
    {
      packages = {
        bytestring.revision = (((hackage.bytestring)."0.10.8.2").revisions).default;
        directory.revision = (((hackage.directory)."1.3.3.0").revisions).default;
        filepath.revision = (((hackage.filepath)."1.4.2.1").revisions).default;
        ghc-prim.revision = (((hackage.ghc-prim)."0.5.3").revisions).default;
        containers.revision = (((hackage.containers)."0.6.0.1").revisions).default;
        base.revision = (((hackage.base)."4.12.0.0").revisions).default;
        time.revision = (((hackage.time)."1.8.0.2").revisions).default;
        deepseq.revision = (((hackage.deepseq)."1.4.4.0").revisions).default;
        rts.revision = (((hackage.rts)."1.0").revisions).default;
        integer-gmp.revision = (((hackage.integer-gmp)."1.0.2.0").revisions).default;
        unix.revision = (((hackage.unix)."2.7.2.2").revisions).default;
        array.revision = (((hackage.array)."0.5.3.0").revisions).default;
        };
      compiler = {
        version = "8.6.5";
        nix-name = "ghc865";
        packages = {
          "array" = "0.5.3.0";
          "bytestring" = "0.10.8.2";
          "filepath" = "1.4.2.1";
          "ghc-prim" = "0.5.3";
          "base" = "4.12.0.0";
          "time" = "1.8.0.2";
          "directory" = "1.3.3.0";
          "rts" = "1.0";
          "deepseq" = "1.4.4.0";
          "unix" = "2.7.2.2";
          "integer-gmp" = "1.0.2.0";
          "containers" = "0.6.0.1";
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
    ({ lib, ... }:
      {
        packages = {
          "filepath".components.library.planned = lib.mkOverride 900 true;
          "bytestring".components.library.planned = lib.mkOverride 900 true;
          "ghc-prim".components.library.planned = lib.mkOverride 900 true;
          "array".components.library.planned = lib.mkOverride 900 true;
          "rts".components.library.planned = lib.mkOverride 900 true;
          "unix".components.library.planned = lib.mkOverride 900 true;
          "directory".components.library.planned = lib.mkOverride 900 true;
          "time".components.library.planned = lib.mkOverride 900 true;
          "alex".components.exes."alex".planned = lib.mkOverride 900 true;
          "deepseq".components.library.planned = lib.mkOverride 900 true;
          "base".components.library.planned = lib.mkOverride 900 true;
          "integer-gmp".components.library.planned = lib.mkOverride 900 true;
          "containers".components.library.planned = lib.mkOverride 900 true;
          };
        })
    ];
  }