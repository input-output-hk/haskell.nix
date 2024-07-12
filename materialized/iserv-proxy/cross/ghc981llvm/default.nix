{
  pkgs = hackage:
    {
      packages = {
        ghc-prim.revision = hackage.ghc-prim."0.11.0".revisions.default;
        transformers.revision = hackage.transformers."0.6.1.0".revisions.default;
        time.revision = hackage.time."1.12.2".revisions.default;
        base.revision = hackage.base."4.19.0.0".revisions.default;
        unix.revision = hackage.unix."2.8.3.0".revisions.default;
        ghc-boot-th.revision = hackage.ghc-boot-th."9.8.1".revisions.default;
        mtl.revision = hackage.mtl."2.3.1".revisions.default;
        pretty.revision = hackage.pretty."1.1.3.6".revisions.default;
        process.revision = hackage.process."1.6.18.0".revisions.default;
        stm.revision = hackage.stm."2.5.2.1".revisions.default;
        hsc2hs.revision = import ./cabal-files/hsc2hs.nix;
        hsc2hs.flags.in-ghc-tree = false;
        template-haskell.revision = hackage.template-haskell."2.21.0.0".revisions.default;
        ghci.revision = hackage.ghci."9.8.1".revisions.default;
        exceptions.revision = hackage.exceptions."0.10.7".revisions.default;
        ghc-heap.revision = hackage.ghc-heap."9.8.1".revisions.default;
        deepseq.revision = hackage.deepseq."1.5.0.0".revisions.default;
        ghc-boot.revision = hackage.ghc-boot."9.8.1".revisions.default;
        containers.revision = hackage.containers."0.6.8".revisions.default;
        array.revision = hackage.array."0.5.6.0".revisions.default;
        bytestring.revision = hackage.bytestring."0.12.0.2".revisions.default;
        directory.revision = hackage.directory."1.3.8.1".revisions.default;
        network.revision = import ./cabal-files/network.nix;
        network.flags.devel = false;
        ghc-bignum.revision = hackage.ghc-bignum."1.3".revisions.default;
        binary.revision = hackage.binary."0.8.9.1".revisions.default;
        filepath.revision = hackage.filepath."1.4.100.4".revisions.default;
      };
      compiler = {
        version = "9.8.1";
        nix-name = "ghc981";
        packages = {
          "unix" = "2.8.3.0";
          "filepath" = "1.4.100.4";
          "transformers" = "0.6.1.0";
          "bytestring" = "0.12.0.2";
          "containers" = "0.6.8";
          "ghc-prim" = "0.11.0";
          "mtl" = "2.3.1";
          "ghc-boot-th" = "9.8.1";
          "base" = "4.19.0.0";
          "time" = "1.12.2";
          "stm" = "2.5.2.1";
          "ghc-bignum" = "1.3";
          "directory" = "1.3.8.1";
          "ghci" = "9.8.1";
          "template-haskell" = "2.21.0.0";
          "process" = "1.6.18.0";
          "ghc-boot" = "9.8.1";
          "binary" = "0.8.9.1";
          "ghc-heap" = "9.8.1";
          "pretty" = "1.1.3.6";
          "deepseq" = "1.5.0.0";
          "array" = "0.5.6.0";
          "exceptions" = "0.10.7";
        };
      };
    };
  extras = hackage:
    { packages = { iserv-proxy = ./.plan.nix/iserv-proxy.nix; }; };
  modules = [
    ({ lib, ... }:
      { packages = { "iserv-proxy" = { flags = {}; }; }; })
    ({ lib, ... }:
      {
        packages = {
          "directory".components.library.planned = lib.mkOverride 900 true;
          "deepseq".components.library.planned = lib.mkOverride 900 true;
          "iserv-proxy".components.library.planned = lib.mkOverride 900 true;
          "ghc-heap".components.library.planned = lib.mkOverride 900 true;
          "ghc-boot".components.library.planned = lib.mkOverride 900 true;
          "base".components.library.planned = lib.mkOverride 900 true;
          "filepath".components.library.planned = lib.mkOverride 900 true;
          "transformers".components.library.planned = lib.mkOverride 900 true;
          "iserv-proxy".components.exes."iserv-proxy-interpreter".planned = lib.mkOverride 900 true;
          "hsc2hs".components.exes."hsc2hs".planned = lib.mkOverride 900 true;
          "network".components.library.planned = lib.mkOverride 900 true;
          "mtl".components.library.planned = lib.mkOverride 900 true;
          "containers".components.library.planned = lib.mkOverride 900 true;
          "ghc-prim".components.library.planned = lib.mkOverride 900 true;
          "pretty".components.library.planned = lib.mkOverride 900 true;
          "bytestring".components.library.planned = lib.mkOverride 900 true;
          "time".components.library.planned = lib.mkOverride 900 true;
          "template-haskell".components.library.planned = lib.mkOverride 900 true;
          "process".components.library.planned = lib.mkOverride 900 true;
          "iserv-proxy".components.exes."iserv-proxy".planned = lib.mkOverride 900 true;
          "ghc-bignum".components.library.planned = lib.mkOverride 900 true;
          "stm".components.library.planned = lib.mkOverride 900 true;
          "binary".components.library.planned = lib.mkOverride 900 true;
          "exceptions".components.library.planned = lib.mkOverride 900 true;
          "array".components.library.planned = lib.mkOverride 900 true;
          "ghc-boot-th".components.library.planned = lib.mkOverride 900 true;
          "ghci".components.library.planned = lib.mkOverride 900 true;
          "unix".components.library.planned = lib.mkOverride 900 true;
        };
      })
  ];
}