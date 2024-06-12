{
  pkgs = hackage:
    {
      packages = {
        ghc-prim.revision = hackage.ghc-prim."0.7.0".revisions.default;
        transformers.revision = hackage.transformers."0.5.6.2".revisions.default;
        time.revision = hackage.time."1.9.3".revisions.default;
        base.revision = hackage.base."4.15.0.0".revisions.default;
        ghc-boot-th.revision = hackage.ghc-boot-th."9.0.1".revisions.default;
        pretty.revision = hackage.pretty."1.1.3.6".revisions.default;
        template-haskell.revision = hackage.template-haskell."2.17.0.0".revisions.default;
        ghci.revision = hackage.ghci."9.0.1".revisions.default;
        ghc-heap.revision = hackage.ghc-heap."9.0.1".revisions.default;
        deepseq.revision = hackage.deepseq."1.4.5.0".revisions.default;
        ghc-boot.revision = hackage.ghc-boot."9.0.1".revisions.default;
        integer-gmp.revision = hackage.integer-gmp."1.1".revisions.default;
        containers.revision = hackage.containers."0.6.4.1".revisions.default;
        array.revision = hackage.array."0.5.4.0".revisions.default;
        Win32.revision = hackage.Win32."2.10.0.0".revisions.default;
        bytestring.revision = hackage.bytestring."0.10.12.1".revisions.default;
        directory.revision = hackage.directory."1.3.6.1".revisions.default;
        network.revision = import ./cabal-files/network.nix;
        ghc-bignum.revision = hackage.ghc-bignum."1.0".revisions.default;
        binary.revision = hackage.binary."0.8.8.0".revisions.default;
        filepath.revision = hackage.filepath."1.4.2.1".revisions.default;
      };
      compiler = {
        version = "9.0.1";
        nix-name = "ghc901";
        packages = {
          "filepath" = "1.4.2.1";
          "transformers" = "0.5.6.2";
          "bytestring" = "0.10.12.1";
          "containers" = "0.6.4.1";
          "ghc-prim" = "0.7.0";
          "ghc-boot-th" = "9.0.1";
          "base" = "4.15.0.0";
          "time" = "1.9.3";
          "ghc-bignum" = "1.0";
          "directory" = "1.3.6.1";
          "integer-gmp" = "1.1";
          "ghci" = "9.0.1";
          "template-haskell" = "2.17.0.0";
          "ghc-boot" = "9.0.1";
          "binary" = "0.8.8.0";
          "ghc-heap" = "9.0.1";
          "pretty" = "1.1.3.6";
          "deepseq" = "1.4.5.0";
          "Win32" = "2.10.0.0";
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
        "ghc-boot-th"
        "pretty"
        "template-haskell"
        "ghci"
        "ghc-heap"
        "deepseq"
        "ghc-boot"
        "integer-gmp"
        "containers"
        "array"
        "Win32"
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
          "integer-gmp".components.library.planned = lib.mkOverride 900 true;
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
          "Win32".components.library.planned = lib.mkOverride 900 true;
          "hpc".components.library.planned = lib.mkOverride 900 true;
          "ghc-boot-th".components.library.planned = lib.mkOverride 900 true;
          "ghci".components.library.planned = lib.mkOverride 900 true;
        };
      })
  ];
}