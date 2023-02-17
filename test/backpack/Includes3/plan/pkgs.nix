{
  pkgs = hackage:
    {
      packages = {
        ghc-bignum.revision = (((hackage.ghc-bignum)."1.2").revisions).default;
        ghc-prim.revision = (((hackage.ghc-prim)."0.8.0").revisions).default;
        containers.revision = (((hackage.containers)."0.6.5.1").revisions).default;
        base.revision = (((hackage.base)."4.16.4.0").revisions).default;
        deepseq.revision = (((hackage.deepseq)."1.4.6.1").revisions).default;
        rts.revision = (((hackage.rts)."1.0.2").revisions).default;
        array.revision = (((hackage.array)."0.5.4.0").revisions).default;
        };
      compiler = {
        version = "9.2.5";
        nix-name = "ghc925";
        packages = {
          "array" = "0.5.4.0";
          "ghc-prim" = "0.8.0";
          "base" = "4.16.4.0";
          "ghc-bignum" = "1.2";
          "rts" = "1.0.2";
          "deepseq" = "1.4.6.1";
          "containers" = "0.6.5.1";
          };
        };
      };
  extras = hackage:
    { packages = { Includes3 = ./.plan.nix/Includes3.nix; }; };
  modules = [
    ({ lib, ... }:
      { packages = { "Includes3" = { flags = {}; }; }; })
    ({ lib, ... }:
      {
        packages = {
          "ghc-prim".components.library.planned = lib.mkOverride 900 true;
          "array".components.library.planned = lib.mkOverride 900 true;
          "rts".components.library.planned = lib.mkOverride 900 true;
          "Includes3".components.sublibs."sigs".planned = lib.mkOverride 900 true;
          "Includes3".components.sublibs."indef".planned = lib.mkOverride 900 true;
          "ghc-bignum".components.library.planned = lib.mkOverride 900 true;
          "Includes3".components.sublibs."indef+6YmfnARghEC3uazvHVLJ9b".planned = lib.mkOverride 900 true;
          "deepseq".components.library.planned = lib.mkOverride 900 true;
          "base".components.library.planned = lib.mkOverride 900 true;
          "Includes3".components.exes."exe".planned = lib.mkOverride 900 true;
          "containers".components.library.planned = lib.mkOverride 900 true;
          };
        })
    ];
  }