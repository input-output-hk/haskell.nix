{
  pkgs = hackage:
    {
      packages = {
        Cabal-syntax.revision = import ./cabal-files/Cabal-syntax.nix;
        bytestring.revision = (((hackage.bytestring)."0.11.4.0").revisions).default;
        safe-exceptions.revision = import ./cabal-files/safe-exceptions.nix;
        echo.revision = import ./cabal-files/echo.nix;
        echo.flags.example = false;
        exceptions.revision = import ./cabal-files/exceptions.nix;
        exceptions.flags.transformers-0-4 = true;
        directory.revision = import ./cabal-files/directory.nix;
        tar.revision = import ./cabal-files/tar.nix;
        tar.flags.old-bytestring = false;
        tar.flags.old-time = false;
        filepath.revision = import ./cabal-files/filepath.nix;
        filepath.flags.cpphs = false;
        network.revision = import ./cabal-files/network.nix;
        network.flags.devel = false;
        mtl.revision = import ./cabal-files/mtl.nix;
        HTTP.revision = import ./cabal-files/HTTP.nix;
        HTTP.flags.warp-tests = false;
        HTTP.flags.network-uri = true;
        HTTP.flags.conduit10 = false;
        HTTP.flags.warn-as-error = false;
        ghc-bignum.revision = (((hackage.ghc-bignum)."1.3").revisions).default;
        zlib.revision = import ./cabal-files/zlib.nix;
        zlib.flags.non-blocking-ffi = false;
        zlib.flags.bundled-c-zlib = false;
        zlib.flags.pkg-config = false;
        ghc-prim.revision = (((hackage.ghc-prim)."0.10.0").revisions).default;
        base16-bytestring.revision = import ./cabal-files/base16-bytestring.nix;
        parsec.revision = import ./cabal-files/parsec.nix;
        lukko.revision = import ./cabal-files/lukko.nix;
        lukko.flags.ofd-locking = true;
        Cabal.revision = import ./cabal-files/Cabal.nix;
        splitmix.revision = import ./cabal-files/splitmix.nix;
        splitmix.flags.optimised-mixer = false;
        ed25519.revision = import ./cabal-files/ed25519.nix;
        ed25519.flags.test-hlint = true;
        ed25519.flags.test-properties = true;
        containers.revision = (((hackage.containers)."0.6.7").revisions).default;
        stm.revision = (((hackage.stm)."2.5.1.0").revisions).default;
        regex-posix.revision = import ./cabal-files/regex-posix.nix;
        regex-posix.flags._regex-posix-clib = false;
        network-uri.revision = import ./cabal-files/network-uri.nix;
        base.revision = (((hackage.base)."4.18.0.0").revisions).default;
        time.revision = (((hackage.time)."1.12.2").revisions).default;
        hackage-security.revision = import ./cabal-files/hackage-security.nix;
        hackage-security.flags.cabal-syntax = true;
        hackage-security.flags.base48 = true;
        hackage-security.flags.lukko = true;
        hackage-security.flags.use-network-uri = true;
        hackage-security.flags.old-directory = false;
        hackage-security.flags.mtl21 = false;
        hsc2hs.revision = import ./cabal-files/hsc2hs.nix;
        hsc2hs.flags.in-ghc-tree = false;
        async.revision = import ./cabal-files/async.nix;
        async.flags.bench = false;
        random.revision = import ./cabal-files/random.nix;
        deepseq.revision = (((hackage.deepseq)."1.4.8.1").revisions).default;
        rts.revision = (((hackage.rts)."1.0.2").revisions).default;
        edit-distance.revision = import ./cabal-files/edit-distance.nix;
        template-haskell.revision = (((hackage.template-haskell)."2.20.0.0").revisions).default;
        binary.revision = (((hackage.binary)."0.8.9.1").revisions).default;
        base64-bytestring.revision = import ./cabal-files/base64-bytestring.nix;
        regex-base.revision = import ./cabal-files/regex-base.nix;
        process.revision = import ./cabal-files/process.nix;
        unix.revision = import ./cabal-files/unix.nix;
        transformers.revision = import ./cabal-files/transformers.nix;
        cabal-install-solver.revision = import ./cabal-files/cabal-install-solver.nix;
        cabal-install-solver.flags.debug-conflict-sets = false;
        cabal-install-solver.flags.debug-expensive-assertions = false;
        cabal-install-solver.flags.debug-tracetree = false;
        text.revision = (((hackage.text)."2.0.2").revisions).default;
        th-compat.revision = import ./cabal-files/th-compat.nix;
        array.revision = (((hackage.array)."0.5.5.0").revisions).default;
        ghc-boot-th.revision = (((hackage.ghc-boot-th)."9.6.2").revisions).default;
        resolv.revision = import ./cabal-files/resolv.nix;
        pretty.revision = (((hackage.pretty)."1.1.3.6").revisions).default;
        hashable.revision = import ./cabal-files/hashable.nix;
        hashable.flags.random-initial-seed = false;
        hashable.flags.integer-gmp = true;
        cryptohash-sha256.revision = import ./cabal-files/cryptohash-sha256.nix;
        cryptohash-sha256.flags.exe = false;
        cryptohash-sha256.flags.use-cbits = true;
        };
      compiler = {
        version = "9.6.2";
        nix-name = "ghc962";
        packages = {
          "pretty" = "1.1.3.6";
          "text" = "2.0.2";
          "array" = "0.5.5.0";
          "bytestring" = "0.11.4.0";
          "stm" = "2.5.1.0";
          "ghc-prim" = "0.10.0";
          "ghc-boot-th" = "9.6.2";
          "base" = "4.18.0.0";
          "time" = "1.12.2";
          "ghc-bignum" = "1.3";
          "rts" = "1.0.2";
          "template-haskell" = "2.20.0.0";
          "deepseq" = "1.4.8.1";
          "binary" = "0.8.9.1";
          "containers" = "0.6.7";
          };
        };
      };
  extras = hackage:
    { packages = { cabal-install = ./.plan.nix/cabal-install.nix; }; };
  modules = [
    ({ lib, ... }:
      {
        packages = {
          "cabal-install" = {
            flags = {
              "lukko" = lib.mkOverride 900 true;
              "native-dns" = lib.mkOverride 900 true;
              };
            };
          };
        })
    ({ lib, ... }:
      {
        packages = {
          "Cabal-syntax".components.library.planned = lib.mkOverride 900 true;
          "base16-bytestring".components.library.planned = lib.mkOverride 900 true;
          "echo".components.library.planned = lib.mkOverride 900 true;
          "filepath".components.library.planned = lib.mkOverride 900 true;
          "ed25519".components.library.planned = lib.mkOverride 900 true;
          "pretty".components.library.planned = lib.mkOverride 900 true;
          "Cabal".components.library.planned = lib.mkOverride 900 true;
          "bytestring".components.library.planned = lib.mkOverride 900 true;
          "zlib".components.library.planned = lib.mkOverride 900 true;
          "cryptohash-sha256".components.library.planned = lib.mkOverride 900 true;
          "exceptions".components.library.planned = lib.mkOverride 900 true;
          "safe-exceptions".components.library.planned = lib.mkOverride 900 true;
          "ghc-prim".components.library.planned = lib.mkOverride 900 true;
          "array".components.library.planned = lib.mkOverride 900 true;
          "cabal-install".components.library.planned = lib.mkOverride 900 true;
          "binary".components.library.planned = lib.mkOverride 900 true;
          "ghc-boot-th".components.library.planned = lib.mkOverride 900 true;
          "splitmix".components.library.planned = lib.mkOverride 900 true;
          "rts".components.library.planned = lib.mkOverride 900 true;
          "unix".components.library.planned = lib.mkOverride 900 true;
          "hsc2hs".components.exes."hsc2hs".planned = lib.mkOverride 900 true;
          "resolv".components.library.planned = lib.mkOverride 900 true;
          "edit-distance".components.library.planned = lib.mkOverride 900 true;
          "regex-base".components.library.planned = lib.mkOverride 900 true;
          "directory".components.library.planned = lib.mkOverride 900 true;
          "time".components.library.planned = lib.mkOverride 900 true;
          "network".components.library.planned = lib.mkOverride 900 true;
          "cabal-install-solver".components.library.planned = lib.mkOverride 900 true;
          "ghc-bignum".components.library.planned = lib.mkOverride 900 true;
          "network-uri".components.library.planned = lib.mkOverride 900 true;
          "regex-posix".components.library.planned = lib.mkOverride 900 true;
          "HTTP".components.library.planned = lib.mkOverride 900 true;
          "process".components.library.planned = lib.mkOverride 900 true;
          "template-haskell".components.library.planned = lib.mkOverride 900 true;
          "stm".components.library.planned = lib.mkOverride 900 true;
          "async".components.library.planned = lib.mkOverride 900 true;
          "th-compat".components.library.planned = lib.mkOverride 900 true;
          "mtl".components.library.planned = lib.mkOverride 900 true;
          "transformers".components.library.planned = lib.mkOverride 900 true;
          "tar".components.library.planned = lib.mkOverride 900 true;
          "parsec".components.library.planned = lib.mkOverride 900 true;
          "deepseq".components.library.planned = lib.mkOverride 900 true;
          "hackage-security".components.library.planned = lib.mkOverride 900 true;
          "text".components.library.planned = lib.mkOverride 900 true;
          "random".components.library.planned = lib.mkOverride 900 true;
          "base".components.library.planned = lib.mkOverride 900 true;
          "containers".components.library.planned = lib.mkOverride 900 true;
          "lukko".components.library.planned = lib.mkOverride 900 true;
          "base64-bytestring".components.library.planned = lib.mkOverride 900 true;
          "hashable".components.library.planned = lib.mkOverride 900 true;
          "cabal-install".components.exes."cabal".planned = lib.mkOverride 900 true;
          };
        })
    ];
  }