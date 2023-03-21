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
    flags = { integer-gmp = true; random-initial-seed = false; };
    package = {
      specVersion = "1.12";
      identifier = { name = "hashable"; version = "1.4.2.0"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Oleg Grenrus <oleg.grenrus@iki.fi>";
      author = "Milan Straka <fox@ucw.cz>\nJohan Tibell <johan.tibell@gmail.com>";
      homepage = "http://github.com/haskell-unordered-containers/hashable";
      url = "";
      synopsis = "A class for types that can be converted to a hash value";
      description = "This package defines a class, 'Hashable', for types that\ncan be converted to a hash value.  This class\nexists for the benefit of hashing-based data\nstructures.  The package provides instances for\nbasic types and a way to combine hash values.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = (([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          ] ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "9.2")) (hsPkgs."base-orphans" or (errorHandler.buildDepError "base-orphans"))) ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "9.4")) (hsPkgs."data-array-byte" or (errorHandler.buildDepError "data-array-byte"))) ++ (if compiler.isGhc && (compiler.version).ge "9"
          then [
            (hsPkgs."ghc-bignum" or (errorHandler.buildDepError "ghc-bignum"))
            ] ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "9.0.2")) (hsPkgs."ghc-bignum-orphans" or (errorHandler.buildDepError "ghc-bignum-orphans"))
          else if flags.integer-gmp
            then [
              (hsPkgs."integer-gmp" or (errorHandler.buildDepError "integer-gmp"))
              ]
            else [
              (hsPkgs."integer-simple" or (errorHandler.buildDepError "integer-simple"))
              ]);
        buildable = true;
        };
      tests = {
        "hashable-tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
            (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."test-framework" or (errorHandler.buildDepError "test-framework"))
            (hsPkgs."test-framework-hunit" or (errorHandler.buildDepError "test-framework-hunit"))
            (hsPkgs."test-framework-quickcheck2" or (errorHandler.buildDepError "test-framework-quickcheck2"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            ] ++ (pkgs.lib).optional (!system.isWindows) (hsPkgs."unix" or (errorHandler.buildDepError "unix"));
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
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/hashable-1.4.2.0.tar.gz";
      sha256 = "1b4000ea82b81f69d46d0af4152c10c6303873510738e24cfc4767760d30e3f8";
      });
    }) // {
    package-description-override = "cabal-version:      1.12\nname:               hashable\nversion:            1.4.2.0\nsynopsis:           A class for types that can be converted to a hash value\ndescription:\n  This package defines a class, 'Hashable', for types that\n  can be converted to a hash value.  This class\n  exists for the benefit of hashing-based data\n  structures.  The package provides instances for\n  basic types and a way to combine hash values.\n\nhomepage:           http://github.com/haskell-unordered-containers/hashable\n\n-- SPDX-License-Identifier : BSD-3-Clause\nlicense:            BSD3\nlicense-file:       LICENSE\nauthor:\n  Milan Straka <fox@ucw.cz>\n  Johan Tibell <johan.tibell@gmail.com>\n\nmaintainer:         Oleg Grenrus <oleg.grenrus@iki.fi>\nbug-reports:\n  https://github.com/haskell-unordered-containers/hashable/issues\n\nstability:          Provisional\ncategory:           Data\nbuild-type:         Simple\ntested-with:\n  GHC ==8.2.2\n   || ==8.4.4\n   || ==8.6.5\n   || ==8.8.3\n   || ==8.10.4\n   || ==8.10.7\n   || ==9.0.1\n   || ==9.0.2\n   || ==9.2.5\n   || ==9.4.4\n\nextra-source-files:\n  CHANGES.md\n  include/HsHashable.h\n  README.md\n\nflag integer-gmp\n  description:\n    Are we using @integer-gmp@ to provide fast Integer instances? No effect on GHC-9.0 or later.\n\n  manual:      False\n  default:     True\n\nflag random-initial-seed\n  description:\n    Randomly initialize the initial seed on each final executable invocation\n    This is useful for catching cases when you rely on (non-existent)\n    stability of hashable's hash functions.\n    This is not a security feature.\n\n  manual:      True\n  default:     False\n\nlibrary\n  exposed-modules:\n    Data.Hashable\n    Data.Hashable.Generic\n    Data.Hashable.Lifted\n\n  other-modules:\n    Data.Hashable.Class\n    Data.Hashable.Generic.Instances\n    Data.Hashable.Imports\n    Data.Hashable.LowLevel\n\n  c-sources:        cbits/fnv.c\n  include-dirs:     include\n  hs-source-dirs:   src\n  build-depends:\n      base        >=4.10.1.0 && <4.18\n    , bytestring  >=0.10.8.2 && <0.12\n    , containers  >=0.5.10.2 && <0.7\n    , deepseq     >=1.4.3.0  && <1.5\n    , filepath    >=1.4.1.2  && <1.5\n    , ghc-prim\n    , text        >=1.2.3.0  && <1.3  || >=2.0 && <2.1\n\n  if !impl(ghc >=9.2)\n    build-depends: base-orphans >=0.8.6 && <0.9\n\n  if !impl(ghc >=9.4)\n    build-depends: data-array-byte >=0.1.0.1 && <0.2\n\n  -- Integer internals\n  if impl(ghc >=9)\n    build-depends: ghc-bignum >=1.0 && <1.4\n\n    if !impl(ghc >=9.0.2)\n      build-depends: ghc-bignum-orphans >=0.1 && <0.2\n\n  else\n    if flag(integer-gmp)\n      build-depends: integer-gmp >=0.4 && <1.1\n\n    else\n      -- this is needed for the automatic flag to be well-balanced\n      build-depends: integer-simple\n\n  if (flag(random-initial-seed) && impl(ghc))\n    cpp-options: -DHASHABLE_RANDOM_SEED=1\n\n    if os(windows)\n      c-sources: cbits-win/init.c\n\n    else\n      c-sources: cbits-unix/init.c\n\n  default-language: Haskell2010\n  other-extensions:\n    BangPatterns\n    CPP\n    DeriveDataTypeable\n    FlexibleContexts\n    FlexibleInstances\n    GADTs\n    KindSignatures\n    MagicHash\n    MultiParamTypeClasses\n    ScopedTypeVariables\n    Trustworthy\n    TypeOperators\n    UnliftedFFITypes\n\n  ghc-options:      -Wall -fwarn-tabs\n\n  if impl(ghc >=9.0)\n    -- these flags may abort compilation with GHC-8.10\n    -- https://gitlab.haskell.org/ghc/ghc/-/merge_requests/3295\n    ghc-options: -Winferred-safe-imports -Wmissing-safe-haskell-mode\n\ntest-suite hashable-tests\n  type:             exitcode-stdio-1.0\n  hs-source-dirs:   tests\n  main-is:          Main.hs\n  other-modules:\n    Properties\n    Regress\n\n  build-depends:\n      base\n    , bytestring\n    , ghc-prim\n    , hashable\n    , HUnit\n    , QuickCheck                  >=2.4.0.1\n    , random                      >=1.0      && <1.3\n    , test-framework              >=0.3.3\n    , test-framework-hunit\n    , test-framework-quickcheck2  >=0.2.9\n    , text                        >=0.11.0.5\n\n  if !os(windows)\n    build-depends:    unix\n    cpp-options:      -DHAVE_MMAP\n    other-modules:    Regress.Mmap\n    other-extensions: CApiFFI\n\n  ghc-options:      -Wall -fno-warn-orphans\n  default-language: Haskell2010\n\ntest-suite hashable-examples\n  type:             exitcode-stdio-1.0\n  build-depends:\n      base\n    , ghc-prim\n    , hashable\n\n  hs-source-dirs:   examples\n  main-is:          Main.hs\n  default-language: Haskell2010\n\nsource-repository head\n  type:     git\n  location:\n    https://github.com/haskell-unordered-containers/hashable.git\n";
    }