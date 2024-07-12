{
  pkgs = hackage:
    {
      packages = {
        ghc-prim.revision = hackage.ghc-prim."0.8.0".revisions.default;
        transformers.revision = hackage.transformers."0.5.6.2".revisions.default;
        time.revision = hackage.time."1.11.1.1".revisions.default;
        base.revision = hackage.base."4.16.4.0".revisions.default;
        unix.revision = hackage.unix."2.7.2.2".revisions.default;
        ghc-boot-th.revision = hackage.ghc-boot-th."9.2.7".revisions.default;
        pretty.revision = hackage.pretty."1.1.3.6".revisions.default;
        template-haskell.revision = hackage.template-haskell."2.18.0.0".revisions.default;
        ghci.revision = hackage.ghci."9.2.7".revisions.default;
        ghc-heap.revision = hackage.ghc-heap."9.2.7".revisions.default;
        deepseq.revision = hackage.deepseq."1.4.6.1".revisions.default;
        ghc-boot.revision = hackage.ghc-boot."9.2.7".revisions.default;
        containers.revision = hackage.containers."0.6.5.1".revisions.default;
        array.revision = hackage.array."0.5.4.0".revisions.default;
        bytestring.revision = hackage.bytestring."0.11.4.0".revisions.default;
        directory.revision = hackage.directory."1.3.6.2".revisions.default;
        network.revision = import ./cabal-files/network.nix;
        ghc-bignum.revision = hackage.ghc-bignum."1.3".revisions.default;
        binary.revision = hackage.binary."0.8.9.0".revisions.default;
        filepath.revision = hackage.filepath."1.4.2.2".revisions.default;
      };
      compiler = {
        version = "9.2.7";
        nix-name = "ghc927";
        packages = {
          "unix" = "2.7.2.2";
          "filepath" = "1.4.2.2";
          "transformers" = "0.5.6.2";
          "bytestring" = "0.11.4.0";
          "containers" = "0.6.5.1";
          "ghc-prim" = "0.8.0";
          "ghc-boot-th" = "9.2.7";
          "base" = "4.16.4.0";
          "time" = "1.11.1.1";
          "ghc-bignum" = "1.3";
          "directory" = "1.3.6.2";
          "ghci" = "9.2.7";
          "template-haskell" = "2.18.0.0";
          "ghc-boot" = "9.2.7";
          "binary" = "0.8.9.0";
          "ghc-heap" = "9.2.7";
          "pretty" = "1.1.3.6";
          "deepseq" = "1.4.6.1";
          "array" = "0.5.4.0";
        };
      };
    };
  extras = hackage:
    {
      packages = {
        libiserv = ./.plan.nix/libiserv.nix;
        remote-iserv = ./.plan.nix/remote-iserv.nix;
        iserv-proxy = ./.plan.nix/iserv-proxy.nix;
        iserv = ./.plan.nix/iserv.nix;
        hpc = ./.plan.nix/hpc.nix;
      };
    };
  modules = [
    {
      preExistingPkgs = [
        "ghc-prim"
        "transformers"
        "time"
        "base"
        "unix"
        "ghc-boot-th"
        "pretty"
        "template-haskell"
        "ghci"
        "ghc-heap"
        "deepseq"
        "ghc-boot"
        "containers"
        "array"
        "bytestring"
        "directory"
        "ghc-bignum"
        "binary"
        "filepath"
      ];
    }
    ({ lib, ... }:
      {
        packages = {
          "libiserv" = { flags = { "network" = lib.mkOverride 900 true; }; };
          "remote-iserv" = { flags = {}; };
          "iserv-proxy" = { flags = {}; };
          "iserv" = { flags = {}; };
          "hpc" = { flags = {}; };
        };
      })
    ({ lib, ... }:
      {
        packages = {
          "directory".components.library.planned = lib.mkOverride 900 true;
          "deepseq".components.library.planned = lib.mkOverride 900 true;
          "ghc-heap".components.library.planned = lib.mkOverride 900 true;
          "ghc-boot".components.library.planned = lib.mkOverride 900 true;
          "libiserv".components.library.planned = lib.mkOverride 900 true;
          "base".components.library.planned = lib.mkOverride 900 true;
          "filepath".components.library.planned = lib.mkOverride 900 true;
          "remote-iserv".components.exes."remote-iserv".planned = lib.mkOverride 900 true;
          "transformers".components.library.planned = lib.mkOverride 900 true;
          "network".components.library.planned = lib.mkOverride 900 true;
          "containers".components.library.planned = lib.mkOverride 900 true;
          "ghc-prim".components.library.planned = lib.mkOverride 900 true;
          "pretty".components.library.planned = lib.mkOverride 900 true;
          "iserv".components.exes."iserv".planned = lib.mkOverride 900 true;
          "bytestring".components.library.planned = lib.mkOverride 900 true;
          "time".components.library.planned = lib.mkOverride 900 true;
          "template-haskell".components.library.planned = lib.mkOverride 900 true;
          "iserv-proxy".components.exes."iserv-proxy".planned = lib.mkOverride 900 true;
          "ghc-bignum".components.library.planned = lib.mkOverride 900 true;
          "binary".components.library.planned = lib.mkOverride 900 true;
          "array".components.library.planned = lib.mkOverride 900 true;
          "hpc".components.library.planned = lib.mkOverride 900 true;
          "ghc-boot-th".components.library.planned = lib.mkOverride 900 true;
          "ghci".components.library.planned = lib.mkOverride 900 true;
          "unix".components.library.planned = lib.mkOverride 900 true;
        };
      })
  ];
}