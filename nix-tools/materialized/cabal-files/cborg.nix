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
    flags = { optimize-gmp = true; };
    package = {
      specVersion = "1.10";
      identifier = { name = "cborg"; version = "0.2.9.0"; };
      license = "BSD-3-Clause";
      copyright = "2015-2019 Duncan Coutts,\n2015-2019 Well-Typed LLP,\n2015 IRIS Connect Ltd";
      maintainer = "duncan@community.haskell.org, ben@smart-cactus.org";
      author = "Duncan Coutts";
      homepage = "";
      url = "";
      synopsis = "Concise Binary Object Representation (CBOR)";
      description = "This package provides an efficient implementation of the Concise\nBinary Object Representation (CBOR), as specified by\n[RFC 7049](https://tools.ietf.org/html/rfc7049).\n\nIf you are looking for a library for serialisation of Haskell values,\nhave a look at the [serialise](/package/serialise) package, which is\nbuilt upon this library.\n\nAn implementation of the standard bijection between CBOR and JSON is\nprovided by the [cborg-json](/package/cborg-json) package. Also see\n[cbor-tool](/package/cbor-tool) for a convenient command-line utility\nfor working with CBOR data.\n\nThis package was formerly known as @binary-serialise-cbor@.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = ([
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
          (hsPkgs."half" or (errorHandler.buildDepError "half"))
          (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          ] ++ (pkgs.lib).optionals (flags.optimize-gmp) (if compiler.isGhc && (compiler.version).ge "9.0"
          then [
            (hsPkgs."ghc-bignum" or (errorHandler.buildDepError "ghc-bignum"))
            ]
          else [
            (hsPkgs."integer-gmp" or (errorHandler.buildDepError "integer-gmp"))
            ])) ++ (pkgs.lib).optionals (!(compiler.isGhc && (compiler.version).ge "8.0")) [
          (hsPkgs."fail" or (errorHandler.buildDepError "fail"))
          (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          ];
        buildable = true;
        };
      tests = {
        "tests" = {
          depends = [
            (hsPkgs."array" or (errorHandler.buildDepError "array"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base-orphans" or (errorHandler.buildDepError "base-orphans"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
            (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."base64-bytestring" or (errorHandler.buildDepError "base64-bytestring"))
            (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."half" or (errorHandler.buildDepError "half"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            ] ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8.0")) (hsPkgs."fail" or (errorHandler.buildDepError "fail"));
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/cborg-0.2.9.0.tar.gz";
      sha256 = "dcb48d1723f06c0340ddad0cf0140dd6da528601605db3faaa8322e0c16afcaa";
      });
    }) // {
    package-description-override = "name:                cborg\nversion:             0.2.9.0\nsynopsis:            Concise Binary Object Representation (CBOR)\nlicense:             BSD3\nlicense-file:        LICENSE.txt\nauthor:              Duncan Coutts\nmaintainer:          duncan@community.haskell.org, ben@smart-cactus.org\nbug-reports:         https://github.com/well-typed/cborg/issues\ncopyright:           2015-2019 Duncan Coutts,\n                     2015-2019 Well-Typed LLP,\n                     2015 IRIS Connect Ltd\ncategory:            Codec\nbuild-type:          Simple\ncabal-version:       >= 1.10\ntested-with:\n  GHC == 8.4.4,\n  GHC == 8.6.5,\n  GHC == 8.8.3,\n  GHC == 8.10.7,\n  GHC == 9.0.1,\n  GHC == 9.2.2,\n  GHC == 9.4.2,\n  GHC == 9.6.1\n\nextra-source-files:\n  ChangeLog.md\n\ndescription:\n  This package provides an efficient implementation of the Concise\n  Binary Object Representation (CBOR), as specified by\n  [RFC 7049](https://tools.ietf.org/html/rfc7049).\n  .\n  If you are looking for a library for serialisation of Haskell values,\n  have a look at the [serialise](/package/serialise) package, which is\n  built upon this library.\n  .\n  An implementation of the standard bijection between CBOR and JSON is\n  provided by the [cborg-json](/package/cborg-json) package. Also see\n  [cbor-tool](/package/cbor-tool) for a convenient command-line utility\n  for working with CBOR data.\n  .\n  This package was formerly known as @binary-serialise-cbor@.\n\nextra-source-files:\n  src/cbits/cbor.h\n  tests/test-vectors/appendix_a.json\n  tests/test-vectors/README.md\n\nsource-repository head\n  type: git\n  location: https://github.com/well-typed/cborg.git\n\n--------------------------------------------------------------------------------\n-- Flags\n\nflag optimize-gmp\n  default: True\n  manual: False\n  description: Use optimized code paths for integer-gmp\n\n--------------------------------------------------------------------------------\n-- Library\n\nlibrary\n  default-language:  Haskell2010\n  ghc-options:       -Wall\n  include-dirs:      src/cbits\n  hs-source-dirs:    src\n\n  exposed-modules:\n    Codec.CBOR\n    Codec.CBOR.Decoding\n    Codec.CBOR.Encoding\n    Codec.CBOR.FlatTerm\n    Codec.CBOR.Magic\n    Codec.CBOR.Pretty\n    Codec.CBOR.Read\n    Codec.CBOR.Write\n    Codec.CBOR.Term\n    Codec.CBOR.ByteArray\n    Codec.CBOR.ByteArray.Sliced\n\n  other-modules:\n    Codec.CBOR.ByteArray.Internal\n\n  other-extensions:\n    CPP, ForeignFunctionInterface, MagicHash,\n    UnboxedTuples, BangPatterns, DeriveDataTypeable,\n    RankNTypes\n\n  build-depends:\n    array                   >= 0.4     && < 0.6,\n    base                    >= 4.11    && < 4.19,\n    bytestring              >= 0.10.4  && < 0.12,\n    containers              >= 0.5     && < 0.7,\n    deepseq                 >= 1.0     && < 1.5,\n    ghc-prim                >= 0.3.1.0 && < 0.11,\n    half                    >= 0.2.2.3 && < 0.4,\n    primitive               >= 0.5     && < 0.9,\n    text                    >= 1.1     && < 1.3 || >= 2.0 && <2.1\n\n  if flag(optimize-gmp)\n    cpp-options:            -DOPTIMIZE_GMP\n    if impl(ghc >= 9.0)\n      cpp-options:          -DHAVE_GHC_BIGNUM\n      build-depends: ghc-bignum >= 1.0 && < 2.0\n    else\n      build-depends: integer-gmp >= 1.0 && < 2.0\n\n  if impl(ghc >= 8.0)\n    ghc-options: -Wcompat -Wnoncanonical-monad-instances\n  else\n    build-depends:\n      -- provide/emulate `Control.Monad.Fail` and `Data.Semigroups` API for pre-GHC8\n      fail                    == 4.9.*,\n      semigroups              >= 0.18 && < 0.21,\n      -- the `PS` pattern synonym in bytestring 0.11 is unavailable with GHC < 8.0\n      bytestring              < 0.11\n\ntest-suite tests\n  type:              exitcode-stdio-1.0\n  hs-source-dirs:    tests\n  main-is:           Main.hs\n\n  default-language:  Haskell2010\n  ghc-options:\n    -Wall -fno-warn-orphans\n    -threaded -rtsopts \"-with-rtsopts=-N2\"\n\n  other-modules:\n    Tests.UnitTests\n    Tests.Properties\n    Tests.Boundary\n    Tests.ByteOffset\n    Tests.Canonical\n    Tests.PreEncoded\n    Tests.Regress\n    Tests.Regress.Issue160\n    Tests.Regress.Issue162\n    Tests.Regress.FlatTerm\n    Tests.Reference\n    Tests.Reference.Implementation\n    Tests.Reference.Generators\n    Tests.Reference.TestVectors\n    Tests.Term\n    Tests.UTF8\n    Tests.Util\n\n  build-depends:\n    array                   >= 0.4     && < 0.6,\n    base                    >= 4.11    && < 4.19,\n    base-orphans,\n    bytestring              >= 0.10.4  && < 0.12,\n    text                    >= 1.1     && < 2.1,\n    primitive               >= 0.5     && < 0.9,\n    cborg,\n    aeson                   >= 0.7     && < 2.2,\n    base64-bytestring       >= 1.0     && < 1.3,\n    base16-bytestring       >= 1.0     && < 1.1,\n    deepseq                 >= 1.0     && < 1.5,\n    half                    >= 0.2.2.3 && < 0.4,\n    QuickCheck              >= 2.9     && < 2.15,\n    random,\n    scientific              >= 0.3     && < 0.4,\n    tasty                   >= 0.11    && < 1.5,\n    tasty-hunit             >= 0.9     && < 0.11,\n    tasty-quickcheck        >= 0.8     && < 0.11,\n    vector                  >= 0.10    && < 0.14\n  if !impl(ghc >= 8.0)\n    build-depends:\n      fail                    >= 4.9.0.0 && < 4.10\n";
    }