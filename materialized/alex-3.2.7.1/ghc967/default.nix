{
  pkgs = hackage:
    {
      packages = {
        filepath.revision = hackage.filepath."1.4.301.0".revisions.default;
        ghc-bignum.revision = hackage.ghc-bignum."1.3".revisions.default;
        stm.revision = hackage.stm."2.5.1.0".revisions.default;
        transformers.revision = hackage.transformers."0.6.1.0".revisions.default;
        deepseq.revision = hackage.deepseq."1.4.8.1".revisions.default;
        directory.revision = hackage.directory."1.3.8.5".revisions.default;
        mtl.revision = hackage.mtl."2.3.1".revisions.default;
        base.revision = hackage.base."4.18.3.0".revisions.default;
        time.revision = hackage.time."1.12.2".revisions.default;
        array.revision = hackage.array."0.5.8.0".revisions.default;
        alex.revision = import ./cabal-files/alex.nix;
        template-haskell.revision = hackage.template-haskell."2.20.0.0".revisions.default;
        unix.revision = hackage.unix."2.8.6.0".revisions.default;
        exceptions.revision = hackage.exceptions."0.10.7".revisions.default;
        bytestring.revision = hackage.bytestring."0.11.5.4".revisions.default;
        ghc-boot-th.revision = hackage.ghc-boot-th."9.6.7".revisions.default;
        ghc-prim.revision = hackage.ghc-prim."0.10.0".revisions.default;
        pretty.revision = hackage.pretty."1.1.3.6".revisions.default;
        containers.revision = hackage.containers."0.6.7".revisions.default;
      };
      compiler = {
        version = "9.6.7";
        nix-name = "ghc967";
        packages = {
          "ghc-boot-th" = "9.6.7";
          "pretty" = "1.1.3.6";
          "array" = "0.5.8.0";
          "time" = "1.12.2";
          "ghc-prim" = "0.10.0";
          "bytestring" = "0.11.5.4";
          "mtl" = "2.3.1";
          "template-haskell" = "2.20.0.0";
          "ghc-bignum" = "1.3";
          "stm" = "2.5.1.0";
          "filepath" = "1.4.301.0";
          "unix" = "2.8.6.0";
          "exceptions" = "0.10.7";
          "deepseq" = "1.4.8.1";
          "transformers" = "0.6.1.0";
          "containers" = "0.6.7";
          "base" = "4.18.3.0";
          "directory" = "1.3.8.5";
        };
      };
    };
  extras = hackage:
    { packages = {}; };
  modules = [
    {
      preExistingPkgs = [
        "filepath"
        "ghc-bignum"
        "stm"
        "transformers"
        "deepseq"
        "directory"
        "mtl"
        "base"
        "time"
        "array"
        "template-haskell"
        "unix"
        "exceptions"
        "bytestring"
        "ghc-boot-th"
        "ghc-prim"
        "pretty"
        "containers"
      ];
    }
    ({ lib, ... }:
      { packages = {}; })
    ({ lib, ... }:
      {
        packages = {
          "alex".components.exes."alex".planned = lib.mkOverride 900 true;
          "template-haskell".components.library.planned = lib.mkOverride 900 true;
          "transformers".components.library.planned = lib.mkOverride 900 true;
          "array".components.library.planned = lib.mkOverride 900 true;
          "unix".components.library.planned = lib.mkOverride 900 true;
          "exceptions".components.library.planned = lib.mkOverride 900 true;
          "directory".components.library.planned = lib.mkOverride 900 true;
          "containers".components.library.planned = lib.mkOverride 900 true;
          "stm".components.library.planned = lib.mkOverride 900 true;
          "bytestring".components.library.planned = lib.mkOverride 900 true;
          "deepseq".components.library.planned = lib.mkOverride 900 true;
          "filepath".components.library.planned = lib.mkOverride 900 true;
          "time".components.library.planned = lib.mkOverride 900 true;
          "ghc-bignum".components.library.planned = lib.mkOverride 900 true;
          "pretty".components.library.planned = lib.mkOverride 900 true;
          "mtl".components.library.planned = lib.mkOverride 900 true;
          "ghc-boot-th".components.library.planned = lib.mkOverride 900 true;
          "base".components.library.planned = lib.mkOverride 900 true;
          "ghc-prim".components.library.planned = lib.mkOverride 900 true;
        };
      })
  ];
}