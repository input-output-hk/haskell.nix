{
  pkgs = hackage:
    {
      packages = {
        bytestring.revision = (((hackage.bytestring)."0.11.4.0").revisions).default;
        exceptions.revision = (((hackage.exceptions)."0.10.7").revisions).default;
        directory.revision = (((hackage.directory)."1.3.8.1").revisions).default;
        filepath.revision = (((hackage.filepath)."1.4.100.1").revisions).default;
        network.revision = import ./cabal-files/network.nix;
        network.flags.devel = false;
        mtl.revision = (((hackage.mtl)."2.3.1").revisions).default;
        ghc-bignum.revision = (((hackage.ghc-bignum)."1.3").revisions).default;
        ghc-prim.revision = (((hackage.ghc-prim)."0.10.0").revisions).default;
        ghc-heap.revision = (((hackage.ghc-heap)."9.8").revisions).default;
        splitmix.revision = import ./cabal-files/splitmix.nix;
        splitmix.flags.optimised-mixer = false;
        containers.revision = (((hackage.containers)."0.6.7").revisions).default;
        stm.revision = (((hackage.stm)."2.5.1.0").revisions).default;
        base.revision = (((hackage.base)."4.18.0.0").revisions).default;
        time.revision = (((hackage.time)."1.12.2").revisions).default;
        Win32.revision = (((hackage.Win32)."2.13.4.0").revisions).default;
        hsc2hs.revision = import ./cabal-files/hsc2hs.nix;
        hsc2hs.flags.in-ghc-tree = false;
        random.revision = import ./cabal-files/random.nix;
        deepseq.revision = (((hackage.deepseq)."1.4.8.1").revisions).default;
        rts.revision = (((hackage.rts)."1.0.2").revisions).default;
        temporary.revision = import ./cabal-files/temporary.nix;
        ghci.revision = (((hackage.ghci)."9.8").revisions).default;
        template-haskell.revision = (((hackage.template-haskell)."2.20.0.0").revisions).default;
        binary.revision = (((hackage.binary)."0.8.9.1").revisions).default;
        ghc-boot.revision = (((hackage.ghc-boot)."9.8").revisions).default;
        process.revision = (((hackage.process)."1.6.17.0").revisions).default;
        transformers.revision = (((hackage.transformers)."0.6.1.0").revisions).default;
        array.revision = (((hackage.array)."0.5.5.0").revisions).default;
        ghc-boot-th.revision = (((hackage.ghc-boot-th)."9.8").revisions).default;
        pretty.revision = (((hackage.pretty)."1.1.3.6").revisions).default;
        };
      compiler = {
        version = "9.8.20230704";
        nix-name = "ghc9820230704";
        packages = {
          "ghc-boot" = "9.8";
          "pretty" = "1.1.3.6";
          "array" = "0.5.5.0";
          "mtl" = "2.3.1";
          "bytestring" = "0.11.4.0";
          "filepath" = "1.4.100.1";
          "stm" = "2.5.1.0";
          "ghc-heap" = "9.8";
          "ghc-prim" = "0.10.0";
          "ghc-boot-th" = "9.8";
          "base" = "4.18.0.0";
          "time" = "1.12.2";
          "Win32" = "2.13.4.0";
          "process" = "1.6.17.0";
          "ghc-bignum" = "1.3";
          "directory" = "1.3.8.1";
          "exceptions" = "0.10.7";
          "rts" = "1.0.2";
          "transformers" = "0.6.1.0";
          "template-haskell" = "2.20.0.0";
          "ghci" = "9.8";
          "deepseq" = "1.4.8.1";
          "binary" = "0.8.9.1";
          "containers" = "0.6.7";
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
          "filepath".components.library.planned = lib.mkOverride 900 true;
          "pretty".components.library.planned = lib.mkOverride 900 true;
          "bytestring".components.library.planned = lib.mkOverride 900 true;
          "exceptions".components.library.planned = lib.mkOverride 900 true;
          "ghc-prim".components.library.planned = lib.mkOverride 900 true;
          "array".components.library.planned = lib.mkOverride 900 true;
          "Win32".components.library.planned = lib.mkOverride 900 true;
          "binary".components.library.planned = lib.mkOverride 900 true;
          "ghc-boot-th".components.library.planned = lib.mkOverride 900 true;
          "splitmix".components.library.planned = lib.mkOverride 900 true;
          "rts".components.library.planned = lib.mkOverride 900 true;
          "temporary".components.library.planned = lib.mkOverride 900 true;
          "hsc2hs".components.exes."hsc2hs".planned = lib.mkOverride 900 true;
          "directory".components.library.planned = lib.mkOverride 900 true;
          "time".components.library.planned = lib.mkOverride 900 true;
          "iserv-proxy".components.library.planned = lib.mkOverride 900 true;
          "network".components.library.planned = lib.mkOverride 900 true;
          "ghc-bignum".components.library.planned = lib.mkOverride 900 true;
          "process".components.library.planned = lib.mkOverride 900 true;
          "template-haskell".components.library.planned = lib.mkOverride 900 true;
          "stm".components.library.planned = lib.mkOverride 900 true;
          "ghci".components.library.planned = lib.mkOverride 900 true;
          "ghc-boot".components.library.planned = lib.mkOverride 900 true;
          "ghc-heap".components.library.planned = lib.mkOverride 900 true;
          "mtl".components.library.planned = lib.mkOverride 900 true;
          "transformers".components.library.planned = lib.mkOverride 900 true;
          "deepseq".components.library.planned = lib.mkOverride 900 true;
          "random".components.library.planned = lib.mkOverride 900 true;
          "base".components.library.planned = lib.mkOverride 900 true;
          "iserv-proxy".components.exes."iserv-proxy-interpreter".planned = lib.mkOverride 900 true;
          "containers".components.library.planned = lib.mkOverride 900 true;
          "iserv-proxy".components.exes."iserv-proxy".planned = lib.mkOverride 900 true;
          };
        })
    ];
  }