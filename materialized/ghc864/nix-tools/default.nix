{
  pkgs = hackage:
    {
      packages = {
        "happy".revision = (((hackage."happy")."1.20.0").revisions).default;
        "streaming-commons".revision = (((hackage."streaming-commons")."0.2.2.3").revisions).default;
        "streaming-commons".flags.use-bytestring-builder = false;
        "pretty".revision = (((hackage."pretty")."1.1.3.6").revisions).default;
        "cborg".revision = (((hackage."cborg")."0.2.6.0").revisions).default;
        "cborg".flags.optimize-gmp = true;
        "network-uri".revision = (((hackage."network-uri")."2.6.4.1").revisions).default;
        "unordered-containers".revision = (((hackage."unordered-containers")."0.2.16.0").revisions).default;
        "unordered-containers".flags.debug = false;
        "integer-logarithms".revision = (((hackage."integer-logarithms")."1.0.3.1").revisions).default;
        "integer-logarithms".flags.check-bounds = false;
        "integer-logarithms".flags.integer-gmp = true;
        "hourglass".revision = (((hackage."hourglass")."0.2.12").revisions).default;
        "dlist".revision = (((hackage."dlist")."1.0").revisions).default;
        "dlist".flags.werror = false;
        "text".revision = (((hackage."text")."1.2.3.1").revisions).default;
        "hnix-store-core".revision = (((hackage."hnix-store-core")."0.5.0.0").revisions).default;
        "hnix-store-core".flags.bounded_memory = false;
        "array".revision = (((hackage."array")."0.5.3.0").revisions).default;
        "base64-bytestring".revision = (((hackage."base64-bytestring")."1.2.1.0").revisions).default;
        "network".revision = (((hackage."network")."3.1.2.7").revisions).default;
        "network".flags.devel = false;
        "asn1-parse".revision = (((hackage."asn1-parse")."0.9.5").revisions).default;
        "parser-combinators".revision = (((hackage."parser-combinators")."1.3.0").revisions).default;
        "parser-combinators".flags.dev = false;
        "cryptonite".revision = (((hackage."cryptonite")."0.29").revisions).default;
        "cryptonite".flags.check_alignment = false;
        "cryptonite".flags.support_sse = false;
        "cryptonite".flags.use_target_attributes = true;
        "cryptonite".flags.support_deepseq = true;
        "cryptonite".flags.support_rdrand = true;
        "cryptonite".flags.old_toolchain_inliner = false;
        "cryptonite".flags.integer-gmp = true;
        "cryptonite".flags.support_pclmuldq = false;
        "cryptonite".flags.support_aesni = true;
        "vector".revision = (((hackage."vector")."0.12.3.1").revisions).default;
        "vector".flags.internalchecks = false;
        "vector".flags.wall = false;
        "vector".flags.boundschecks = true;
        "vector".flags.unsafechecks = false;
        "socks".revision = (((hackage."socks")."0.6.1").revisions).default;
        "http-types".revision = (((hackage."http-types")."0.12.3").revisions).default;
        "some".revision = (((hackage."some")."1.0.3").revisions).default;
        "some".flags.newtype-unsafe = true;
        "microlens-aeson".revision = (((hackage."microlens-aeson")."2.4.1").revisions).default;
        "comonad".revision = (((hackage."comonad")."5.0.8").revisions).default;
        "comonad".flags.containers = true;
        "comonad".flags.distributive = true;
        "comonad".flags.indexed-traversable = true;
        "x509".revision = (((hackage."x509")."1.7.6").revisions).default;
        "monadlist".revision = (((hackage."monadlist")."0.0.2").revisions).default;
        "profunctors".revision = (((hackage."profunctors")."5.6.2").revisions).default;
        "x509-system".revision = (((hackage."x509-system")."1.6.7").revisions).default;
        "appar".revision = (((hackage."appar")."0.1.8").revisions).default;
        "extra".revision = (((hackage."extra")."1.7.10").revisions).default;
        "asn1-types".revision = (((hackage."asn1-types")."0.3.4").revisions).default;
        "base-compat".revision = (((hackage."base-compat")."0.12.1").revisions).default;
        "contravariant".revision = (((hackage."contravariant")."1.5.5").revisions).default;
        "contravariant".flags.tagged = true;
        "contravariant".flags.semigroups = true;
        "contravariant".flags.statevar = true;
        "base-compat-batteries".revision = (((hackage."base-compat-batteries")."0.12.1").revisions).default;
        "yaml".revision = (((hackage."yaml")."0.11.7.0").revisions).default;
        "yaml".flags.no-examples = true;
        "yaml".flags.no-exe = true;
        "th-lift-instances".revision = (((hackage."th-lift-instances")."0.1.19").revisions).default;
        "Cabal".revision = (((hackage."Cabal")."3.6.2.0").revisions).default;
        "Cabal".flags.bundled-binary-generic = false;
        "assoc".revision = (((hackage."assoc")."1.0.2").revisions).default;
        "data-fix".revision = (((hackage."data-fix")."0.3.2").revisions).default;
        "tls".revision = (((hackage."tls")."1.5.7").revisions).default;
        "tls".flags.network = true;
        "tls".flags.hans = false;
        "tls".flags.compat = true;
        "neat-interpolation".revision = (((hackage."neat-interpolation")."0.5.1.3").revisions).default;
        "http-client-tls".revision = (((hackage."http-client-tls")."0.3.5.3").revisions).default;
        "infer-license".revision = (((hackage."infer-license")."0.2.0").revisions).default;
        "basement".revision = (((hackage."basement")."0.0.12").revisions).default;
        "cryptohash-sha256".revision = (((hackage."cryptohash-sha256")."0.11.102.1").revisions).default;
        "cryptohash-sha256".flags.exe = false;
        "cryptohash-sha256".flags.use-cbits = true;
        "mtl".revision = (((hackage."mtl")."2.2.2").revisions).default;
        "OneTuple".revision = (((hackage."OneTuple")."0.3.1").revisions).default;
        "mime-types".revision = (((hackage."mime-types")."0.1.0.9").revisions).default;
        "serialise".revision = (((hackage."serialise")."0.2.4.0").revisions).default;
        "serialise".flags.newtime15 = true;
        "parsec".revision = (((hackage."parsec")."3.1.13.0").revisions).default;
        "bytestring".revision = (((hackage."bytestring")."0.10.8.2").revisions).default;
        "gitrev".revision = (((hackage."gitrev")."1.3.1").revisions).default;
        "pem".revision = (((hackage."pem")."0.2.4").revisions).default;
        "strict".revision = (((hackage."strict")."0.4.0.1").revisions).default;
        "strict".flags.assoc = true;
        "aeson".revision = (((hackage."aeson")."2.0.3.0").revisions).default;
        "aeson".flags.ordered-keymap = true;
        "aeson".flags.cffi = false;
        "zlib".revision = (((hackage."zlib")."0.6.2.3").revisions).default;
        "zlib".flags.non-blocking-ffi = false;
        "zlib".flags.bundled-c-zlib = false;
        "zlib".flags.pkg-config = false;
        "tagged".revision = (((hackage."tagged")."0.8.6.1").revisions).default;
        "tagged".flags.deepseq = true;
        "tagged".flags.transformers = true;
        "splitmix".revision = (((hackage."splitmix")."0.1.0.4").revisions).default;
        "splitmix".flags.optimised-mixer = false;
        "hpack".revision = (((hackage."hpack")."0.34.6").revisions).default;
        "attoparsec".revision = (((hackage."attoparsec")."0.14.4").revisions).default;
        "attoparsec".flags.developer = false;
        "tar".revision = (((hackage."tar")."0.5.1.1").revisions).default;
        "tar".flags.old-bytestring = false;
        "tar".flags.old-time = false;
        "th-compat".revision = (((hackage."th-compat")."0.1.3").revisions).default;
        "memory".revision = (((hackage."memory")."0.16.0").revisions).default;
        "memory".flags.support_basement = true;
        "memory".flags.support_deepseq = true;
        "memory".flags.support_bytestring = true;
        "memory".flags.support_foundation = true;
        "filepath".revision = (((hackage."filepath")."1.4.2.1").revisions).default;
        "th-lift".revision = (((hackage."th-lift")."0.8.2").revisions).default;
        "libyaml".revision = (((hackage."libyaml")."0.1.2").revisions).default;
        "libyaml".flags.system-libyaml = false;
        "libyaml".flags.no-unicode = false;
        "unliftio-core".revision = (((hackage."unliftio-core")."0.2.0.1").revisions).default;
        "hnix".revision = (((hackage."hnix")."0.16.0").revisions).default;
        "hnix".flags.profiling = false;
        "hnix".flags.optimize = true;
        "stm".revision = (((hackage."stm")."2.5.0.0").revisions).default;
        "free".revision = (((hackage."free")."5.1.7").revisions).default;
        "connection".revision = (((hackage."connection")."0.3.1").revisions).default;
        "haskell-lexer".revision = (((hackage."haskell-lexer")."1.1").revisions).default;
        "microlens".revision = (((hackage."microlens")."0.4.12.0").revisions).default;
        "resourcet".revision = (((hackage."resourcet")."1.2.4.3").revisions).default;
        "unix-compat".revision = (((hackage."unix-compat")."0.5.4").revisions).default;
        "unix-compat".flags.old-time = false;
        "case-insensitive".revision = (((hackage."case-insensitive")."1.2.1.0").revisions).default;
        "byteorder".revision = (((hackage."byteorder")."1.0.4").revisions).default;
        "ghc-prim".revision = (((hackage."ghc-prim")."0.5.3").revisions).default;
        "x509-validation".revision = (((hackage."x509-validation")."1.6.12").revisions).default;
        "ghc-boot-th".revision = (((hackage."ghc-boot-th")."8.6.4").revisions).default;
        "asn1-encoding".revision = (((hackage."asn1-encoding")."0.9.6").revisions).default;
        "indexed-traversable".revision = (((hackage."indexed-traversable")."0.1.2").revisions).default;
        "distributive".revision = (((hackage."distributive")."0.6.2.1").revisions).default;
        "distributive".flags.tagged = true;
        "distributive".flags.semigroups = true;
        "lens-family-th".revision = (((hackage."lens-family-th")."0.5.2.1").revisions).default;
        "text-short".revision = (((hackage."text-short")."0.1.5").revisions).default;
        "text-short".flags.asserts = false;
        "regex-tdfa".revision = (((hackage."regex-tdfa")."1.3.1.1").revisions).default;
        "regex-tdfa".flags.force-o2 = false;
        "bifunctors".revision = (((hackage."bifunctors")."5.5.11").revisions).default;
        "bifunctors".flags.tagged = true;
        "bifunctors".flags.semigroups = true;
        "lens-family-core".revision = (((hackage."lens-family-core")."2.1.0").revisions).default;
        "base".revision = (((hackage."base")."4.12.0.0").revisions).default;
        "nix-derivation".revision = (((hackage."nix-derivation")."1.1.2").revisions).default;
        "time".revision = (((hackage."time")."1.8.0.2").revisions).default;
        "async".revision = (((hackage."async")."2.2.4").revisions).default;
        "async".flags.bench = false;
        "random".revision = (((hackage."random")."1.2.1").revisions).default;
        "cookie".revision = (((hackage."cookie")."0.4.5").revisions).default;
        "process".revision = (((hackage."process")."1.6.5.0").revisions).default;
        "cereal".revision = (((hackage."cereal")."0.5.8.2").revisions).default;
        "cereal".flags.bytestring-builder = false;
        "utf8-string".revision = (((hackage."utf8-string")."1.0.2").revisions).default;
        "megaparsec".revision = (((hackage."megaparsec")."9.0.1").revisions).default;
        "megaparsec".flags.dev = false;
        "base16-bytestring".revision = (((hackage."base16-bytestring")."1.0.2.0").revisions).default;
        "conduit".revision = (((hackage."conduit")."1.3.4.2").revisions).default;
        "transformers-base".revision = (((hackage."transformers-base")."0.4.6").revisions).default;
        "transformers-base".flags.orphaninstances = true;
        "data-default-class".revision = (((hackage."data-default-class")."0.1.2.0").revisions).default;
        "regex-base".revision = (((hackage."regex-base")."0.94.0.2").revisions).default;
        "vector-algorithms".revision = (((hackage."vector-algorithms")."0.8.0.4").revisions).default;
        "vector-algorithms".flags.internalchecks = false;
        "vector-algorithms".flags.llvm = false;
        "vector-algorithms".flags.properties = true;
        "vector-algorithms".flags.boundschecks = true;
        "vector-algorithms".flags.unsafechecks = false;
        "vector-algorithms".flags.bench = true;
        "th-abstraction".revision = (((hackage."th-abstraction")."0.4.3.0").revisions).default;
        "semigroupoids".revision = (((hackage."semigroupoids")."5.3.7").revisions).default;
        "semigroupoids".flags.tagged = true;
        "semigroupoids".flags.containers = true;
        "semigroupoids".flags.distributive = true;
        "semigroupoids".flags.unordered-containers = true;
        "semigroupoids".flags.contravariant = true;
        "semigroupoids".flags.comonad = true;
        "hsc2hs".revision = (((hackage."hsc2hs")."0.68.8").revisions).default;
        "hsc2hs".flags.in-ghc-tree = false;
        "text-metrics".revision = (((hackage."text-metrics")."0.3.0").revisions).default;
        "text-metrics".flags.dev = false;
        "xml".revision = (((hackage."xml")."1.3.14").revisions).default;
        "logict".revision = (((hackage."logict")."0.7.0.3").revisions).default;
        "these".revision = (((hackage."these")."1.1.1.1").revisions).default;
        "these".flags.assoc = true;
        "split".revision = (((hackage."split")."0.2.3.4").revisions).default;
        "base-orphans".revision = (((hackage."base-orphans")."0.8.6").revisions).default;
        "saltine".revision = (((hackage."saltine")."0.2.0.0").revisions).default;
        "time-compat".revision = (((hackage."time-compat")."1.9.6.1").revisions).default;
        "time-compat".flags.old-locale = false;
        "primitive".revision = (((hackage."primitive")."0.7.3.0").revisions).default;
        "directory".revision = (((hackage."directory")."1.3.3.0").revisions).default;
        "exceptions".revision = (((hackage."exceptions")."0.10.4").revisions).default;
        "exceptions".flags.transformers-0-4 = true;
        "half".revision = (((hackage."half")."0.3.1").revisions).default;
        "optparse-applicative".revision = (((hackage."optparse-applicative")."0.16.1.0").revisions).default;
        "optparse-applicative".flags.process = true;
        "lifted-base".revision = (((hackage."lifted-base")."0.2.3.12").revisions).default;
        "clock".revision = (((hackage."clock")."0.8.2").revisions).default;
        "clock".flags.llvm = false;
        "hashing".revision = (((hackage."hashing")."0.1.0.1").revisions).default;
        "prettyprinter".revision = (((hackage."prettyprinter")."1.7.1").revisions).default;
        "prettyprinter".flags.buildreadme = false;
        "prettyprinter".flags.text = true;
        "hnix-store-remote".revision = (((hackage."hnix-store-remote")."0.5.0.0").revisions).default;
        "hnix-store-remote".flags.io-testsuite = false;
        "rts".revision = (((hackage."rts")."1.0").revisions).default;
        "ref-tf".revision = (((hackage."ref-tf")."0.5.0.1").revisions).default;
        "monad-control".revision = (((hackage."monad-control")."1.0.3.1").revisions).default;
        "semialign".revision = (((hackage."semialign")."1.2.0.1").revisions).default;
        "semialign".flags.semigroupoids = true;
        "transformers".revision = (((hackage."transformers")."0.5.6.2").revisions).default;
        "template-haskell".revision = (((hackage."template-haskell")."2.14.0.0").revisions).default;
        "pretty-show".revision = (((hackage."pretty-show")."1.10").revisions).default;
        "mono-traversable".revision = (((hackage."mono-traversable")."1.0.15.1").revisions).default;
        "witherable".revision = (((hackage."witherable")."0.4.2").revisions).default;
        "syb".revision = (((hackage."syb")."0.7.2.1").revisions).default;
        "aeson-pretty".revision = (((hackage."aeson-pretty")."0.8.9").revisions).default;
        "aeson-pretty".flags.lib-only = false;
        "algebraic-graphs".revision = (((hackage."algebraic-graphs")."0.5").revisions).default;
        "lens-family".revision = (((hackage."lens-family")."2.1.1").revisions).default;
        "deepseq".revision = (((hackage."deepseq")."1.4.4.0").revisions).default;
        "unix".revision = (((hackage."unix")."2.7.2.2").revisions).default;
        "ansi-terminal".revision = (((hackage."ansi-terminal")."0.11.1").revisions).default;
        "ansi-terminal".flags.example = false;
        "blaze-builder".revision = (((hackage."blaze-builder")."0.4.2.2").revisions).default;
        "x509-store".revision = (((hackage."x509-store")."1.6.9").revisions).default;
        "deriving-compat".revision = (((hackage."deriving-compat")."0.6").revisions).default;
        "deriving-compat".flags.new-functor-classes = true;
        "deriving-compat".flags.template-haskell-2-11 = true;
        "deriving-compat".flags.base-4-9 = true;
        "hashable".revision = (((hackage."hashable")."1.3.5.0").revisions).default;
        "hashable".flags.random-initial-seed = false;
        "hashable".flags.integer-gmp = true;
        "integer-gmp".revision = (((hackage."integer-gmp")."1.0.2.0").revisions).default;
        "cmdargs".revision = (((hackage."cmdargs")."0.10.21").revisions).default;
        "cmdargs".flags.quotation = true;
        "cmdargs".flags.testprog = false;
        "transformers-compat".revision = (((hackage."transformers-compat")."0.7.1").revisions).default;
        "transformers-compat".flags.two = false;
        "transformers-compat".flags.mtl = true;
        "transformers-compat".flags.four = false;
        "transformers-compat".flags.five = false;
        "transformers-compat".flags.five-three = true;
        "transformers-compat".flags.three = false;
        "transformers-compat".flags.generic-deriving = true;
        "indexed-traversable-instances".revision = (((hackage."indexed-traversable-instances")."0.1.1").revisions).default;
        "iproute".revision = (((hackage."iproute")."1.7.12").revisions).default;
        "relude".revision = (((hackage."relude")."1.0.0.1").revisions).default;
        "scientific".revision = (((hackage."scientific")."0.3.7.0").revisions).default;
        "scientific".flags.bytestring-builder = false;
        "scientific".flags.integer-simple = false;
        "binary".revision = (((hackage."binary")."0.8.6.0").revisions).default;
        "http-client".revision = (((hackage."http-client")."0.7.10").revisions).default;
        "http-client".flags.network-uri = true;
        "ansi-wl-pprint".revision = (((hackage."ansi-wl-pprint")."0.6.9").revisions).default;
        "ansi-wl-pprint".flags.example = false;
        "QuickCheck".revision = (((hackage."QuickCheck")."2.14.2").revisions).default;
        "QuickCheck".flags.old-random = false;
        "QuickCheck".flags.templatehaskell = true;
        "uuid-types".revision = (((hackage."uuid-types")."1.0.5").revisions).default;
        "containers".revision = (((hackage."containers")."0.6.0.1").revisions).default;
        "StateVar".revision = (((hackage."StateVar")."1.2.2").revisions).default;
        "Glob".revision = (((hackage."Glob")."0.10.2").revisions).default;
        "colour".revision = (((hackage."colour")."2.3.5").revisions).default;
        };
      compiler = {
        version = "8.6.4";
        nix-name = "ghc864";
        packages = {
          "pretty" = "1.1.3.6";
          "text" = "1.2.3.1";
          "array" = "0.5.3.0";
          "mtl" = "2.2.2";
          "parsec" = "3.1.13.0";
          "bytestring" = "0.10.8.2";
          "filepath" = "1.4.2.1";
          "stm" = "2.5.0.0";
          "ghc-prim" = "0.5.3";
          "ghc-boot-th" = "8.6.4";
          "base" = "4.12.0.0";
          "time" = "1.8.0.2";
          "process" = "1.6.5.0";
          "directory" = "1.3.3.0";
          "rts" = "1.0";
          "transformers" = "0.5.6.2";
          "template-haskell" = "2.14.0.0";
          "deepseq" = "1.4.4.0";
          "unix" = "2.7.2.2";
          "integer-gmp" = "1.0.2.0";
          "binary" = "0.8.6.0";
          "containers" = "0.6.0.1";
          };
        };
      };
  extras = hackage:
    {
      packages = {
        nix-tools = ./.plan.nix/nix-tools.nix;
        hackage-db = ./.plan.nix/hackage-db.nix;
        };
      };
  modules = [
    ({ lib, ... }:
      {
        packages = {
          "nix-tools" = { flags = {}; };
          "hackage-db" = {
            flags = { "install-examples" = lib.mkOverride 900 false; };
            };
          };
        })
    ({ lib, ... }:
      {
        packages = {
          "ansi-terminal".components.library.planned = lib.mkOverride 900 true;
          "http-client-tls".components.library.planned = lib.mkOverride 900 true;
          "base16-bytestring".components.library.planned = lib.mkOverride 900 true;
          "transformers-base".components.library.planned = lib.mkOverride 900 true;
          "base-orphans".components.library.planned = lib.mkOverride 900 true;
          "socks".components.library.planned = lib.mkOverride 900 true;
          "megaparsec".components.library.planned = lib.mkOverride 900 true;
          "cookie".components.library.planned = lib.mkOverride 900 true;
          "these".components.library.planned = lib.mkOverride 900 true;
          "cereal".components.library.planned = lib.mkOverride 900 true;
          "pretty-show".components.exes."ppsh".planned = lib.mkOverride 900 true;
          "resourcet".components.library.planned = lib.mkOverride 900 true;
          "extra".components.library.planned = lib.mkOverride 900 true;
          "microlens".components.library.planned = lib.mkOverride 900 true;
          "filepath".components.library.planned = lib.mkOverride 900 true;
          "pretty-show".components.library.planned = lib.mkOverride 900 true;
          "distributive".components.library.planned = lib.mkOverride 900 true;
          "pretty".components.library.planned = lib.mkOverride 900 true;
          "utf8-string".components.library.planned = lib.mkOverride 900 true;
          "nix-derivation".components.exes."pretty-derivation".planned = lib.mkOverride 900 true;
          "Cabal".components.library.planned = lib.mkOverride 900 true;
          "gitrev".components.library.planned = lib.mkOverride 900 true;
          "bytestring".components.library.planned = lib.mkOverride 900 true;
          "nix-tools".components.exes."hashes-to-nix".planned = lib.mkOverride 900 true;
          "mono-traversable".components.library.planned = lib.mkOverride 900 true;
          "zlib".components.library.planned = lib.mkOverride 900 true;
          "strict".components.library.planned = lib.mkOverride 900 true;
          "comonad".components.library.planned = lib.mkOverride 900 true;
          "data-fix".components.library.planned = lib.mkOverride 900 true;
          "microlens-aeson".components.library.planned = lib.mkOverride 900 true;
          "cryptohash-sha256".components.library.planned = lib.mkOverride 900 true;
          "cborg".components.library.planned = lib.mkOverride 900 true;
          "serialise".components.library.planned = lib.mkOverride 900 true;
          "xml".components.library.planned = lib.mkOverride 900 true;
          "profunctors".components.library.planned = lib.mkOverride 900 true;
          "exceptions".components.library.planned = lib.mkOverride 900 true;
          "dlist".components.library.planned = lib.mkOverride 900 true;
          "ghc-prim".components.library.planned = lib.mkOverride 900 true;
          "some".components.library.planned = lib.mkOverride 900 true;
          "array".components.library.planned = lib.mkOverride 900 true;
          "x509".components.library.planned = lib.mkOverride 900 true;
          "binary".components.library.planned = lib.mkOverride 900 true;
          "ghc-boot-th".components.library.planned = lib.mkOverride 900 true;
          "scientific".components.library.planned = lib.mkOverride 900 true;
          "relude".components.library.planned = lib.mkOverride 900 true;
          "splitmix".components.library.planned = lib.mkOverride 900 true;
          "rts".components.library.planned = lib.mkOverride 900 true;
          "tagged".components.library.planned = lib.mkOverride 900 true;
          "unix".components.library.planned = lib.mkOverride 900 true;
          "monadlist".components.library.planned = lib.mkOverride 900 true;
          "hsc2hs".components.exes."hsc2hs".planned = lib.mkOverride 900 true;
          "parser-combinators".components.library.planned = lib.mkOverride 900 true;
          "vector".components.library.planned = lib.mkOverride 900 true;
          "indexed-traversable-instances".components.library.planned = lib.mkOverride 900 true;
          "data-default-class".components.library.planned = lib.mkOverride 900 true;
          "regex-base".components.library.planned = lib.mkOverride 900 true;
          "nix-tools".components.exes."cabal-to-nix".planned = lib.mkOverride 900 true;
          "cryptonite".components.library.planned = lib.mkOverride 900 true;
          "asn1-parse".components.library.planned = lib.mkOverride 900 true;
          "lens-family".components.library.planned = lib.mkOverride 900 true;
          "mime-types".components.library.planned = lib.mkOverride 900 true;
          "hpack".components.exes."hpack".planned = lib.mkOverride 900 true;
          "ref-tf".components.library.planned = lib.mkOverride 900 true;
          "directory".components.library.planned = lib.mkOverride 900 true;
          "happy".components.exes."happy".planned = lib.mkOverride 900 true;
          "nix-tools".components.exes."truncate-index".planned = lib.mkOverride 900 true;
          "time".components.library.planned = lib.mkOverride 900 true;
          "network".components.library.planned = lib.mkOverride 900 true;
          "hashing".components.exes."hashing-exe".planned = lib.mkOverride 900 true;
          "cmdargs".components.library.planned = lib.mkOverride 900 true;
          "aeson-pretty".components.exes."aeson-pretty".planned = lib.mkOverride 900 true;
          "nix-tools".components.exes."hackage-to-nix".planned = lib.mkOverride 900 true;
          "StateVar".components.library.planned = lib.mkOverride 900 true;
          "deriving-compat".components.library.planned = lib.mkOverride 900 true;
          "case-insensitive".components.library.planned = lib.mkOverride 900 true;
          "neat-interpolation".components.library.planned = lib.mkOverride 900 true;
          "saltine".components.library.planned = lib.mkOverride 900 true;
          "half".components.library.planned = lib.mkOverride 900 true;
          "free".components.library.planned = lib.mkOverride 900 true;
          "unix-compat".components.library.planned = lib.mkOverride 900 true;
          "x509-store".components.library.planned = lib.mkOverride 900 true;
          "vector-algorithms".components.library.planned = lib.mkOverride 900 true;
          "blaze-builder".components.library.planned = lib.mkOverride 900 true;
          "asn1-types".components.library.planned = lib.mkOverride 900 true;
          "hashing".components.library.planned = lib.mkOverride 900 true;
          "lifted-base".components.library.planned = lib.mkOverride 900 true;
          "unliftio-core".components.library.planned = lib.mkOverride 900 true;
          "yaml".components.library.planned = lib.mkOverride 900 true;
          "indexed-traversable".components.library.planned = lib.mkOverride 900 true;
          "network-uri".components.library.planned = lib.mkOverride 900 true;
          "memory".components.library.planned = lib.mkOverride 900 true;
          "pem".components.library.planned = lib.mkOverride 900 true;
          "base-compat-batteries".components.library.planned = lib.mkOverride 900 true;
          "hnix".components.library.planned = lib.mkOverride 900 true;
          "split".components.library.planned = lib.mkOverride 900 true;
          "contravariant".components.library.planned = lib.mkOverride 900 true;
          "appar".components.library.planned = lib.mkOverride 900 true;
          "syb".components.library.planned = lib.mkOverride 900 true;
          "hnix-store-core".components.library.planned = lib.mkOverride 900 true;
          "text-short".components.library.planned = lib.mkOverride 900 true;
          "lens-family-th".components.library.planned = lib.mkOverride 900 true;
          "assoc".components.library.planned = lib.mkOverride 900 true;
          "process".components.library.planned = lib.mkOverride 900 true;
          "nix-tools".components.library.planned = lib.mkOverride 900 true;
          "prettyprinter".components.library.planned = lib.mkOverride 900 true;
          "clock".components.library.planned = lib.mkOverride 900 true;
          "template-haskell".components.library.planned = lib.mkOverride 900 true;
          "aeson-pretty".components.library.planned = lib.mkOverride 900 true;
          "th-lift".components.library.planned = lib.mkOverride 900 true;
          "libyaml".components.library.planned = lib.mkOverride 900 true;
          "stm".components.library.planned = lib.mkOverride 900 true;
          "infer-license".components.library.planned = lib.mkOverride 900 true;
          "byteorder".components.library.planned = lib.mkOverride 900 true;
          "witherable".components.library.planned = lib.mkOverride 900 true;
          "asn1-encoding".components.library.planned = lib.mkOverride 900 true;
          "semialign".components.library.planned = lib.mkOverride 900 true;
          "http-client".components.library.planned = lib.mkOverride 900 true;
          "async".components.library.planned = lib.mkOverride 900 true;
          "nix-tools".components.exes."cabal-name".planned = lib.mkOverride 900 true;
          "iproute".components.library.planned = lib.mkOverride 900 true;
          "nix-tools".components.exes."lts-to-nix".planned = lib.mkOverride 900 true;
          "th-compat".components.library.planned = lib.mkOverride 900 true;
          "tls".components.library.planned = lib.mkOverride 900 true;
          "http-types".components.library.planned = lib.mkOverride 900 true;
          "logict".components.library.planned = lib.mkOverride 900 true;
          "nix-tools".components.exes."plan-to-nix".planned = lib.mkOverride 900 true;
          "QuickCheck".components.library.planned = lib.mkOverride 900 true;
          "ansi-wl-pprint".components.library.planned = lib.mkOverride 900 true;
          "uuid-types".components.library.planned = lib.mkOverride 900 true;
          "semigroupoids".components.library.planned = lib.mkOverride 900 true;
          "x509-validation".components.library.planned = lib.mkOverride 900 true;
          "attoparsec".components.library.planned = lib.mkOverride 900 true;
          "algebraic-graphs".components.library.planned = lib.mkOverride 900 true;
          "haskell-lexer".components.library.planned = lib.mkOverride 900 true;
          "mtl".components.library.planned = lib.mkOverride 900 true;
          "th-abstraction".components.library.planned = lib.mkOverride 900 true;
          "text-metrics".components.library.planned = lib.mkOverride 900 true;
          "attoparsec".components.sublibs."attoparsec-internal".planned = lib.mkOverride 900 true;
          "transformers".components.library.planned = lib.mkOverride 900 true;
          "tar".components.library.planned = lib.mkOverride 900 true;
          "OneTuple".components.library.planned = lib.mkOverride 900 true;
          "th-lift-instances".components.library.planned = lib.mkOverride 900 true;
          "parsec".components.library.planned = lib.mkOverride 900 true;
          "deepseq".components.library.planned = lib.mkOverride 900 true;
          "primitive".components.library.planned = lib.mkOverride 900 true;
          "conduit".components.library.planned = lib.mkOverride 900 true;
          "hnix-store-remote".components.library.planned = lib.mkOverride 900 true;
          "text".components.library.planned = lib.mkOverride 900 true;
          "bifunctors".components.library.planned = lib.mkOverride 900 true;
          "unordered-containers".components.library.planned = lib.mkOverride 900 true;
          "random".components.library.planned = lib.mkOverride 900 true;
          "base".components.library.planned = lib.mkOverride 900 true;
          "lens-family-core".components.library.planned = lib.mkOverride 900 true;
          "nix-derivation".components.library.planned = lib.mkOverride 900 true;
          "regex-tdfa".components.library.planned = lib.mkOverride 900 true;
          "integer-logarithms".components.library.planned = lib.mkOverride 900 true;
          "Glob".components.library.planned = lib.mkOverride 900 true;
          "integer-gmp".components.library.planned = lib.mkOverride 900 true;
          "transformers-compat".components.library.planned = lib.mkOverride 900 true;
          "monad-control".components.library.planned = lib.mkOverride 900 true;
          "streaming-commons".components.library.planned = lib.mkOverride 900 true;
          "colour".components.library.planned = lib.mkOverride 900 true;
          "containers".components.library.planned = lib.mkOverride 900 true;
          "nix-tools".components.exes."stack-repos".planned = lib.mkOverride 900 true;
          "time-compat".components.library.planned = lib.mkOverride 900 true;
          "basement".components.library.planned = lib.mkOverride 900 true;
          "optparse-applicative".components.library.planned = lib.mkOverride 900 true;
          "aeson".components.library.planned = lib.mkOverride 900 true;
          "x509-system".components.library.planned = lib.mkOverride 900 true;
          "hourglass".components.library.planned = lib.mkOverride 900 true;
          "base-compat".components.library.planned = lib.mkOverride 900 true;
          "hackage-db".components.library.planned = lib.mkOverride 900 true;
          "base64-bytestring".components.library.planned = lib.mkOverride 900 true;
          "hashable".components.library.planned = lib.mkOverride 900 true;
          "hpack".components.library.planned = lib.mkOverride 900 true;
          "nix-tools".components.exes."stack-to-nix".planned = lib.mkOverride 900 true;
          "connection".components.library.planned = lib.mkOverride 900 true;
          };
        })
    ];
  }