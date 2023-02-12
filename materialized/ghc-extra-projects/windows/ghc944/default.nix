{
  pkgs = hackage:
    {
      packages = {
        Cabal-syntax.revision = (((hackage.Cabal-syntax)."3.8.1.0").revisions).default;
        bytestring.revision = (((hackage.bytestring)."0.11.3.1").revisions).default;
        exceptions.revision = (((hackage.exceptions)."0.10.5").revisions).default;
        directory.revision = (((hackage.directory)."1.3.7.1").revisions).default;
        filepath.revision = (((hackage.filepath)."1.4.2.2").revisions).default;
        mtl.revision = (((hackage.mtl)."2.2.2").revisions).default;
        ghc-bignum.revision = (((hackage.ghc-bignum)."1.3").revisions).default;
        ghc-prim.revision = (((hackage.ghc-prim)."0.9.0").revisions).default;
        parsec.revision = (((hackage.parsec)."3.1.15.0").revisions).default;
        ghc-heap.revision = (((hackage.ghc-heap)."9.4.4").revisions).default;
        Cabal.revision = (((hackage.Cabal)."3.8.1.0").revisions).default;
        containers.revision = (((hackage.containers)."0.6.6").revisions).default;
        stm.revision = (((hackage.stm)."2.5.1.0").revisions).default;
        alex.revision = import ./cabal-files/alex.nix;
        base.revision = (((hackage.base)."4.17.0.0").revisions).default;
        time.revision = (((hackage.time)."1.12.2").revisions).default;
        Win32.revision = (((hackage.Win32)."2.12.0.1").revisions).default;
        deepseq.revision = (((hackage.deepseq)."1.4.8.0").revisions).default;
        happy.revision = import ./cabal-files/happy.nix;
        rts.revision = (((hackage.rts)."1.0.2").revisions).default;
        template-haskell.revision = (((hackage.template-haskell)."2.19.0.0").revisions).default;
        binary.revision = (((hackage.binary)."0.8.9.1").revisions).default;
        process.revision = (((hackage.process)."1.6.16.0").revisions).default;
        transformers.revision = (((hackage.transformers)."0.5.6.2").revisions).default;
        text.revision = (((hackage.text)."2.0.1").revisions).default;
        array.revision = (((hackage.array)."0.5.4.0").revisions).default;
        ghc-boot-th.revision = (((hackage.ghc-boot-th)."9.4.4").revisions).default;
        pretty.revision = (((hackage.pretty)."1.1.3.6").revisions).default;
        };
      compiler = {
        version = "9.4.4";
        nix-name = "ghc944";
        packages = {
          "pretty" = "1.1.3.6";
          "text" = "2.0.1";
          "array" = "0.5.4.0";
          "Cabal-syntax" = "3.8.1.0";
          "Cabal" = "3.8.1.0";
          "mtl" = "2.2.2";
          "parsec" = "3.1.15.0";
          "bytestring" = "0.11.3.1";
          "filepath" = "1.4.2.2";
          "stm" = "2.5.1.0";
          "ghc-heap" = "9.4.4";
          "ghc-prim" = "0.9.0";
          "ghc-boot-th" = "9.4.4";
          "base" = "4.17.0.0";
          "time" = "1.12.2";
          "Win32" = "2.12.0.1";
          "process" = "1.6.16.0";
          "ghc-bignum" = "1.3";
          "directory" = "1.3.7.1";
          "exceptions" = "0.10.5";
          "rts" = "1.0.2";
          "transformers" = "0.5.6.2";
          "template-haskell" = "2.19.0.0";
          "deepseq" = "1.4.8.0";
          "binary" = "0.8.9.1";
          "containers" = "0.6.6";
          };
        };
      };
  extras = hackage:
    {
      packages = {
        ghc = ./.plan.nix/ghc.nix;
        deriveConstants = ./.plan.nix/deriveConstants.nix;
        remote-iserv = ./.plan.nix/remote-iserv.nix;
        ghci = ./.plan.nix/ghci.nix;
        ghc-boot = ./.plan.nix/ghc-boot.nix;
        iserv = ./.plan.nix/iserv.nix;
        genprimopcode = ./.plan.nix/genprimopcode.nix;
        libiserv = ./.plan.nix/libiserv.nix;
        hpc = ./.plan.nix/hpc.nix;
        };
      };
  modules = [
    ({ lib, ... }:
      {
        packages = {
          "ghc" = {
            flags = {
              "dynamic-system-linker" = lib.mkOverride 900 true;
              "terminfo" = lib.mkOverride 900 true;
              "internal-interpreter" = lib.mkOverride 900 false;
              "build-tool-depends" = lib.mkOverride 900 true;
              };
            };
          "deriveConstants" = { flags = {}; };
          "remote-iserv" = { flags = {}; };
          "ghci" = {
            flags = { "internal-interpreter" = lib.mkOverride 900 true; };
            };
          "ghc-boot" = { flags = {}; };
          "iserv" = { flags = {}; };
          "genprimopcode" = {
            flags = { "build-tool-depends" = lib.mkOverride 900 true; };
            };
          "libiserv" = { flags = { "network" = lib.mkOverride 900 true; }; };
          "hpc" = { flags = {}; };
          };
        })
    ({ lib, ... }:
      {
        packages = {
          "Cabal-syntax".components.library.planned = lib.mkOverride 900 true;
          "filepath".components.library.planned = lib.mkOverride 900 true;
          "pretty".components.library.planned = lib.mkOverride 900 true;
          "Cabal".components.library.planned = lib.mkOverride 900 true;
          "bytestring".components.library.planned = lib.mkOverride 900 true;
          "remote-iserv".components.exes."remote-iserv".planned = lib.mkOverride 900 true;
          "exceptions".components.library.planned = lib.mkOverride 900 true;
          "ghc-prim".components.library.planned = lib.mkOverride 900 true;
          "array".components.library.planned = lib.mkOverride 900 true;
          "Win32".components.library.planned = lib.mkOverride 900 true;
          "binary".components.library.planned = lib.mkOverride 900 true;
          "ghc-boot-th".components.library.planned = lib.mkOverride 900 true;
          "rts".components.library.planned = lib.mkOverride 900 true;
          "directory".components.library.planned = lib.mkOverride 900 true;
          "happy".components.exes."happy".planned = lib.mkOverride 900 true;
          "iserv".components.exes."iserv".planned = lib.mkOverride 900 true;
          "ghc".components.setup.planned = lib.mkOverride 900 true;
          "time".components.library.planned = lib.mkOverride 900 true;
          "ghc".components.library.planned = lib.mkOverride 900 true;
          "ghc-bignum".components.library.planned = lib.mkOverride 900 true;
          "process".components.library.planned = lib.mkOverride 900 true;
          "template-haskell".components.library.planned = lib.mkOverride 900 true;
          "stm".components.library.planned = lib.mkOverride 900 true;
          "ghci".components.library.planned = lib.mkOverride 900 true;
          "alex".components.exes."alex".planned = lib.mkOverride 900 true;
          "ghc-boot".components.library.planned = lib.mkOverride 900 true;
          "deriveConstants".components.exes."deriveConstants".planned = lib.mkOverride 900 true;
          "hpc".components.library.planned = lib.mkOverride 900 true;
          "ghc-boot".components.setup.planned = lib.mkOverride 900 true;
          "ghc-heap".components.library.planned = lib.mkOverride 900 true;
          "mtl".components.library.planned = lib.mkOverride 900 true;
          "transformers".components.library.planned = lib.mkOverride 900 true;
          "libiserv".components.library.planned = lib.mkOverride 900 true;
          "parsec".components.library.planned = lib.mkOverride 900 true;
          "deepseq".components.library.planned = lib.mkOverride 900 true;
          "genprimopcode".components.exes."genprimopcode".planned = lib.mkOverride 900 true;
          "text".components.library.planned = lib.mkOverride 900 true;
          "base".components.library.planned = lib.mkOverride 900 true;
          "containers".components.library.planned = lib.mkOverride 900 true;
          };
        })
    ];
  }