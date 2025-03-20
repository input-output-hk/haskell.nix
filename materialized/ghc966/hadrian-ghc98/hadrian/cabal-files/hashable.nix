{ system
  , compiler
  , flags
  , pkgs
  , hsPkgs
  , pkgconfPkgs
  , errorHandler
  , config
  , ... }:
  ({
    flags = { arch-native = false; random-initial-seed = false; };
    package = {
      specVersion = "2.2";
      identifier = { name = "hashable"; version = "1.5.0.0"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Oleg Grenrus <oleg.grenrus@iki.fi>";
      author = "Milan Straka <fox@ucw.cz>\nJohan Tibell <johan.tibell@gmail.com>";
      homepage = "http://github.com/haskell-unordered-containers/hashable";
      url = "";
      synopsis = "A class for types that can be converted to a hash value";
      description = "This package defines a class, 'Hashable', for types that can be converted to a hash value.\nThis class exists for the benefit of hashing-based data structures.\nThe package provides instances for basic types and a way to combine hash values.\n\n'Hashable' is intended exclusively for use in in-memory data structures.\n\n'Hashable' does /not/ have a fixed standard.\nThis allows it to improve over time.\n\nBecause it does not have a fixed standard, different computers or computers on different versions of the code will observe different hash values.\nAs such, 'hashable' is not recommended for use other than in-memory datastructures.\nSpecifically, 'hashable' is not intended for network use or in applications which persist hashed values.\nFor stable hashing use named hashes: sha256, crc32, xxhash etc.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."os-string" or (errorHandler.buildDepError "os-string"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."ghc-bignum" or (errorHandler.buildDepError "ghc-bignum"))
        ];
        buildable = true;
      };
      tests = {
        "hashable-tests" = {
          depends = ([
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
            (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
          ] ++ pkgs.lib.optional (compiler.isGhc && compiler.version.ge "9.2") (hsPkgs."os-string" or (errorHandler.buildDepError "os-string"))) ++ pkgs.lib.optional (!system.isWindows) (hsPkgs."unix" or (errorHandler.buildDepError "unix"));
          buildable = true;
        };
        "xxhash-tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
          ] ++ pkgs.lib.optional (!(compiler.isGhc && compiler.version.ge "9.4")) (hsPkgs."data-array-byte" or (errorHandler.buildDepError "data-array-byte"));
          buildable = true;
        };
        "hashable-examples" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
            (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/hashable-1.5.0.0.tar.gz";
      sha256 = "e58b3a8e18da5f6cd7e937e5fd683e500bb1f8276b3768269759119ca0cddb6a";
    });
  }) // {
    package-description-override = "cabal-version:      2.2\nname:               hashable\nversion:            1.5.0.0\nsynopsis:           A class for types that can be converted to a hash value\ndescription:\n  This package defines a class, 'Hashable', for types that can be converted to a hash value.\n  This class exists for the benefit of hashing-based data structures.\n  The package provides instances for basic types and a way to combine hash values.\n  .\n  'Hashable' is intended exclusively for use in in-memory data structures.\n  .\n  'Hashable' does /not/ have a fixed standard.\n  This allows it to improve over time.\n  .\n  Because it does not have a fixed standard, different computers or computers on different versions of the code will observe different hash values.\n  As such, 'hashable' is not recommended for use other than in-memory datastructures.\n  Specifically, 'hashable' is not intended for network use or in applications which persist hashed values.\n  For stable hashing use named hashes: sha256, crc32, xxhash etc.\n\nhomepage:           http://github.com/haskell-unordered-containers/hashable\nlicense:            BSD-3-Clause\nlicense-file:       LICENSE\nauthor:\n  Milan Straka <fox@ucw.cz>\n  Johan Tibell <johan.tibell@gmail.com>\n\nmaintainer:         Oleg Grenrus <oleg.grenrus@iki.fi>\nbug-reports:\n  https://github.com/haskell-unordered-containers/hashable/issues\n\nstability:          Provisional\ncategory:           Data\nbuild-type:         Simple\ntested-with:        GHC ==9.6.5 || ==9.8.2 || ==9.10.1\nextra-source-files:\n  CHANGES.md\n  include/HsHashable.h\n  include/HsXXHash.h\n  README.md\n  xxHash-0.8.2/xxhash.h\n\nflag arch-native\n  description:\n    Use @-march=native@ when compiling C sources.\n    Portable implementation is 15-50% slower.\n    Consider enabling this flag if hashing performance is important.\n\n  manual:      True\n  default:     False\n\nflag random-initial-seed\n  description:\n    Randomly initialize the initial seed on each final executable invocation\n    This is useful for catching cases when you rely on (non-existent)\n    stability of hashable's hash functions.\n    This is not a security feature.\n\n  manual:      True\n  default:     False\n\nlibrary\n  exposed-modules:\n    Data.Hashable\n    Data.Hashable.Generic\n    Data.Hashable.Lifted\n\n  other-modules:\n    Data.Hashable.Class\n    Data.Hashable.FFI\n    Data.Hashable.Generic.Instances\n    Data.Hashable.Imports\n    Data.Hashable.LowLevel\n    Data.Hashable.Mix\n    Data.Hashable.XXH3\n\n  include-dirs:     include xxHash-0.8.2\n  includes:\n    HsHashable.h\n    HsXXHash.h\n    xxhash.h\n\n  hs-source-dirs:   src\n  build-depends:\n    , base        >=4.18.0.0 && <4.21\n    , bytestring  >=0.11.5.3 && <0.13\n    , containers  >=0.6.7    && <0.8\n    , deepseq     >=1.4.8.1  && <1.6\n    , ghc-prim\n    , text        >=2.0.2    && <2.2\n\n  -- depend on os-string on newer GHCs only.\n  -- os-string has tight lower bound on bytestring, which prevents\n  -- using bundled version on older GHCs.\n  build-depends:    os-string >=2.0.2 && <2.1\n\n  -- we also ensure that we can get filepath-1.5 only with GHC-9.2\n  -- therefore there is else-branch with stricter upper bound.\n  build-depends:    filepath >=1.4.200.1 && <1.6\n\n  -- Integer internals\n  build-depends:    ghc-bignum >=1.3 && <1.4\n\n  if (flag(random-initial-seed) && impl(ghc))\n    cpp-options: -DHASHABLE_RANDOM_SEED=1\n\n    if os(windows)\n      c-sources: cbits-win/init.c\n\n    else\n      c-sources: cbits-unix/init.c\n\n  default-language: Haskell2010\n  other-extensions:\n    BangPatterns\n    CPP\n    DeriveDataTypeable\n    FlexibleContexts\n    FlexibleInstances\n    GADTs\n    KindSignatures\n    MagicHash\n    MultiParamTypeClasses\n    QuantifiedConstraints\n    ScopedTypeVariables\n    Trustworthy\n    TypeOperators\n    UnliftedFFITypes\n\n  ghc-options:      -Wall\n\n  if flag(arch-native)\n    -- Cabal doesn't pass cc-options to \"ordinary\" Haskell source compilation\n    -- https://github.com/haskell/cabal/issues/9801\n    ghc-options: -optc=-march=native -optc-mtune=native\n\n  if impl(ghc >=9.0)\n    -- these flags may abort compilation with GHC-8.10\n    -- https://gitlab.haskell.org/ghc/ghc/-/merge_requests/3295\n    ghc-options: -Winferred-safe-imports -Wmissing-safe-haskell-mode\n\ntest-suite hashable-tests\n  type:             exitcode-stdio-1.0\n  hs-source-dirs:   tests\n  main-is:          Main.hs\n  other-modules:\n    Properties\n    Regress\n\n  build-depends:\n    , base\n    , bytestring\n    , filepath\n    , ghc-prim\n    , hashable\n    , HUnit\n    , QuickCheck        >=2.15\n    , random            >=1.0      && <1.3\n    , tasty             ^>=1.5\n    , tasty-hunit       ^>=0.10.1\n    , tasty-quickcheck  ^>=0.10.3\n    , text              >=0.11.0.5\n\n  if impl(ghc >=9.2)\n    build-depends: os-string\n\n  if !os(windows)\n    build-depends:    unix\n    cpp-options:      -DHAVE_MMAP\n    other-modules:    Regress.Mmap\n    other-extensions: CApiFFI\n\n  ghc-options:      -Wall -fno-warn-orphans\n  default-language: Haskell2010\n\ntest-suite xxhash-tests\n  type:             exitcode-stdio-1.0\n  hs-source-dirs:   tests src\n  main-is:          xxhash-tests.hs\n  other-modules:\n    Data.Hashable.FFI\n    Data.Hashable.XXH3\n\n  default-language: Haskell2010\n  build-depends:\n    , base\n    , bytestring\n    , primitive         ^>=0.9.0.0\n    , tasty             ^>=1.5\n    , tasty-hunit       ^>=0.10.1\n    , tasty-quickcheck  ^>=0.10.3\n\n  include-dirs:     include xxHash-0.8.2\n  includes:\n    HsXXHash.h\n    xxhash.h\n\n  if !impl(ghc >=9.4)\n    build-depends: data-array-byte >=0.1.0.1 && <0.2\n\ntest-suite hashable-examples\n  type:             exitcode-stdio-1.0\n  build-depends:\n    , base\n    , ghc-prim\n    , hashable\n\n  hs-source-dirs:   examples\n  main-is:          Main.hs\n  default-language: Haskell2010\n\nsource-repository head\n  type:     git\n  location:\n    https://github.com/haskell-unordered-containers/hashable.git\n";
  }