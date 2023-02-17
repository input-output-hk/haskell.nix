{
  pkgs = hackage:
    {
      packages = {
        ghc-bignum.revision = (((hackage.ghc-bignum)."1.2").revisions).default;
        ghc-prim.revision = (((hackage.ghc-prim)."0.8.0").revisions).default;
        base.revision = (((hackage.base)."4.16.4.0").revisions).default;
        rts.revision = (((hackage.rts)."1.0.2").revisions).default;
        };
      compiler = {
        version = "9.2.5";
        nix-name = "ghc925";
        packages = {
          "ghc-prim" = "0.8.0";
          "base" = "4.16.4.0";
          "ghc-bignum" = "1.2";
          "rts" = "1.0.2";
          };
        };
      };
  extras = hackage:
    { packages = { Includes2 = ./.plan.nix/Includes2.nix; }; };
  modules = [
    ({ lib, ... }:
      { packages = { "Includes2" = { flags = {}; }; }; })
    ({ lib, ... }:
      {
        packages = {
          "Includes2".components.sublibs."mysql".planned = lib.mkOverride 900 true;
          "Includes2".components.exes."exe".planned = lib.mkOverride 900 true;
          "Includes2".components.sublibs."mylib".planned = lib.mkOverride 900 true;
          "ghc-prim".components.library.planned = lib.mkOverride 900 true;
          "Includes2".components.sublibs."postgresql".planned = lib.mkOverride 900 true;
          "Includes2".components.sublibs."mylib+BfeYWT4IxdwEBRldDzV3Na".planned = lib.mkOverride 900 true;
          "rts".components.library.planned = lib.mkOverride 900 true;
          "ghc-bignum".components.library.planned = lib.mkOverride 900 true;
          "Includes2".components.sublibs."mylib+3gY9SyjX86dBypHcOaev1n".planned = lib.mkOverride 900 true;
          "base".components.library.planned = lib.mkOverride 900 true;
          "Includes2".components.library.planned = lib.mkOverride 900 true;
          };
        })
    ];
  }