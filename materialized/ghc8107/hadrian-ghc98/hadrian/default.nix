{
  pkgs = hackage:
    {
      packages = {
        bytestring.revision = (((hackage.bytestring)."0.10.12.0").revisions).default;
        directory.revision = (((hackage.directory)."1.3.6.0").revisions).default;
        filepath.revision = (((hackage.filepath)."1.4.2.1").revisions).default;
        mtl.revision = (((hackage.mtl)."2.2.2").revisions).default;
        ghc-prim.revision = (((hackage.ghc-prim)."0.6.1").revisions).default;
        base16-bytestring.revision = import ./cabal-files/base16-bytestring.nix;
        parsec.revision = (((hackage.parsec)."3.1.14.0").revisions).default;
        js-flot.revision = import ./cabal-files/js-flot.nix;
        utf8-string.revision = import ./cabal-files/utf8-string.nix;
        Cabal.revision = (((hackage.Cabal)."3.2.1.0").revisions).default;
        splitmix.revision = import ./cabal-files/splitmix.nix;
        splitmix.flags.optimised-mixer = false;
        containers.revision = (((hackage.containers)."0.6.5.1").revisions).default;
        clock.revision = import ./cabal-files/clock.nix;
        clock.flags.llvm = false;
        heaps.revision = import ./cabal-files/heaps.nix;
        base.revision = (((hackage.base)."4.14.3.0").revisions).default;
        time.revision = (((hackage.time)."1.9.3").revisions).default;
        base-orphans.revision = import ./cabal-files/base-orphans.nix;
        random.revision = import ./cabal-files/random.nix;
        primitive.revision = import ./cabal-files/primitive.nix;
        deepseq.revision = (((hackage.deepseq)."1.4.4.0").revisions).default;
        js-jquery.revision = import ./cabal-files/js-jquery.nix;
        js-dgtable.revision = import ./cabal-files/js-dgtable.nix;
        rts.revision = (((hackage.rts)."1.0.1").revisions).default;
        template-haskell.revision = (((hackage.template-haskell)."2.16.0.0").revisions).default;
        binary.revision = (((hackage.binary)."0.8.8.0").revisions).default;
        shake.revision = import ./cabal-files/shake.nix;
        shake.flags.portable = false;
        shake.flags.cloud = false;
        shake.flags.embed-files = false;
        integer-gmp.revision = (((hackage.integer-gmp)."1.0.3.0").revisions).default;
        process.revision = (((hackage.process)."1.6.13.2").revisions).default;
        unix.revision = (((hackage.unix)."2.7.2.2").revisions).default;
        data-array-byte.revision = import ./cabal-files/data-array-byte.nix;
        transformers.revision = (((hackage.transformers)."0.5.6.2").revisions).default;
        unordered-containers.revision = import ./cabal-files/unordered-containers.nix;
        unordered-containers.flags.debug = false;
        QuickCheck.revision = import ./cabal-files/QuickCheck.nix;
        QuickCheck.flags.old-random = false;
        QuickCheck.flags.templatehaskell = true;
        extra.revision = import ./cabal-files/extra.nix;
        text.revision = (((hackage.text)."1.2.4.1").revisions).default;
        array.revision = (((hackage.array)."0.5.4.0").revisions).default;
        ghc-boot-th.revision = (((hackage.ghc-boot-th)."8.10.7").revisions).default;
        filepattern.revision = import ./cabal-files/filepattern.nix;
        pretty.revision = (((hackage.pretty)."1.1.3.6").revisions).default;
        hashable.revision = import ./cabal-files/hashable.nix;
        hashable.flags.random-initial-seed = false;
        hashable.flags.integer-gmp = true;
        cryptohash-sha256.revision = import ./cabal-files/cryptohash-sha256.nix;
        cryptohash-sha256.flags.exe = false;
        cryptohash-sha256.flags.use-cbits = true;
        };
      compiler = {
        version = "8.10.7";
        nix-name = "ghc8107";
        packages = {
          "pretty" = "1.1.3.6";
          "text" = "1.2.4.1";
          "array" = "0.5.4.0";
          "Cabal" = "3.2.1.0";
          "mtl" = "2.2.2";
          "parsec" = "3.1.14.0";
          "bytestring" = "0.10.12.0";
          "filepath" = "1.4.2.1";
          "ghc-prim" = "0.6.1";
          "ghc-boot-th" = "8.10.7";
          "base" = "4.14.3.0";
          "time" = "1.9.3";
          "process" = "1.6.13.2";
          "directory" = "1.3.6.0";
          "rts" = "1.0.1";
          "transformers" = "0.5.6.2";
          "template-haskell" = "2.16.0.0";
          "deepseq" = "1.4.4.0";
          "unix" = "2.7.2.2";
          "integer-gmp" = "1.0.3.0";
          "binary" = "0.8.8.0";
          "containers" = "0.6.5.1";
          };
        };
      };
  extras = hackage:
    { packages = { hadrian = ./.plan.nix/hadrian.nix; }; };
  modules = [
    ({ lib, ... }:
      {
        packages = {
          "hadrian" = {
            flags = {
              "threaded" = lib.mkOverride 900 true;
              "selftest" = lib.mkOverride 900 true;
              };
            };
          };
        })
    ({ lib, ... }:
      {
        packages = {
          "shake".components.library.planned = lib.mkOverride 900 true;
          "base16-bytestring".components.library.planned = lib.mkOverride 900 true;
          "base-orphans".components.library.planned = lib.mkOverride 900 true;
          "heaps".components.library.planned = lib.mkOverride 900 true;
          "extra".components.library.planned = lib.mkOverride 900 true;
          "filepath".components.library.planned = lib.mkOverride 900 true;
          "pretty".components.library.planned = lib.mkOverride 900 true;
          "utf8-string".components.library.planned = lib.mkOverride 900 true;
          "Cabal".components.library.planned = lib.mkOverride 900 true;
          "bytestring".components.library.planned = lib.mkOverride 900 true;
          "cryptohash-sha256".components.library.planned = lib.mkOverride 900 true;
          "ghc-prim".components.library.planned = lib.mkOverride 900 true;
          "array".components.library.planned = lib.mkOverride 900 true;
          "binary".components.library.planned = lib.mkOverride 900 true;
          "filepattern".components.library.planned = lib.mkOverride 900 true;
          "ghc-boot-th".components.library.planned = lib.mkOverride 900 true;
          "splitmix".components.library.planned = lib.mkOverride 900 true;
          "rts".components.library.planned = lib.mkOverride 900 true;
          "unix".components.library.planned = lib.mkOverride 900 true;
          "shake".components.exes."shake".planned = lib.mkOverride 900 true;
          "directory".components.library.planned = lib.mkOverride 900 true;
          "time".components.library.planned = lib.mkOverride 900 true;
          "js-flot".components.library.planned = lib.mkOverride 900 true;
          "data-array-byte".components.library.planned = lib.mkOverride 900 true;
          "process".components.library.planned = lib.mkOverride 900 true;
          "clock".components.library.planned = lib.mkOverride 900 true;
          "template-haskell".components.library.planned = lib.mkOverride 900 true;
          "hadrian".components.exes."hadrian".planned = lib.mkOverride 900 true;
          "QuickCheck".components.library.planned = lib.mkOverride 900 true;
          "mtl".components.library.planned = lib.mkOverride 900 true;
          "transformers".components.library.planned = lib.mkOverride 900 true;
          "parsec".components.library.planned = lib.mkOverride 900 true;
          "deepseq".components.library.planned = lib.mkOverride 900 true;
          "primitive".components.library.planned = lib.mkOverride 900 true;
          "js-jquery".components.library.planned = lib.mkOverride 900 true;
          "text".components.library.planned = lib.mkOverride 900 true;
          "unordered-containers".components.library.planned = lib.mkOverride 900 true;
          "random".components.library.planned = lib.mkOverride 900 true;
          "base".components.library.planned = lib.mkOverride 900 true;
          "integer-gmp".components.library.planned = lib.mkOverride 900 true;
          "containers".components.library.planned = lib.mkOverride 900 true;
          "js-dgtable".components.library.planned = lib.mkOverride 900 true;
          "hashable".components.library.planned = lib.mkOverride 900 true;
          };
        })
    ];
  }