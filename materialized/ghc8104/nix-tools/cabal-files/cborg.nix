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
      identifier = { name = "cborg"; version = "0.2.8.0"; };
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
      url = "http://hackage.haskell.org/package/cborg-0.2.8.0.tar.gz";
      sha256 = "2e59c827c273b57af0fcbbfb62f6d15faeb399e0d89d57baf8adcc60e62ab01e";
      });
    }) // {
    package-description-override = "name:                cborg\r\nversion:             0.2.8.0\r\nx-revision: 1\r\nsynopsis:            Concise Binary Object Representation (CBOR)\r\nlicense:             BSD3\r\nlicense-file:        LICENSE.txt\r\nauthor:              Duncan Coutts\r\nmaintainer:          duncan@community.haskell.org, ben@smart-cactus.org\r\nbug-reports:         https://github.com/well-typed/cborg/issues\r\ncopyright:           2015-2019 Duncan Coutts,\r\n                     2015-2019 Well-Typed LLP,\r\n                     2015 IRIS Connect Ltd\r\ncategory:            Codec\r\nbuild-type:          Simple\r\ncabal-version:       >= 1.10\r\ntested-with:\r\n  GHC == 8.4.4,\r\n  GHC == 8.6.5,\r\n  GHC == 8.8.3,\r\n  GHC == 8.10.7,\r\n  GHC == 9.0.1,\r\n  GHC == 9.2.2,\r\n  GHC == 9.4.2\r\n\r\nextra-source-files:\r\n  ChangeLog.md\r\n\r\ndescription:\r\n  This package provides an efficient implementation of the Concise\r\n  Binary Object Representation (CBOR), as specified by\r\n  [RFC 7049](https://tools.ietf.org/html/rfc7049).\r\n  .\r\n  If you are looking for a library for serialisation of Haskell values,\r\n  have a look at the [serialise](/package/serialise) package, which is\r\n  built upon this library.\r\n  .\r\n  An implementation of the standard bijection between CBOR and JSON is\r\n  provided by the [cborg-json](/package/cborg-json) package. Also see\r\n  [cbor-tool](/package/cbor-tool) for a convenient command-line utility\r\n  for working with CBOR data.\r\n  .\r\n  This package was formerly known as @binary-serialise-cbor@.\r\n\r\nextra-source-files:\r\n  src/cbits/cbor.h\r\n  tests/test-vectors/appendix_a.json\r\n  tests/test-vectors/README.md\r\n\r\nsource-repository head\r\n  type: git\r\n  location: https://github.com/well-typed/cborg.git\r\n\r\n--------------------------------------------------------------------------------\r\n-- Flags\r\n\r\nflag optimize-gmp\r\n  default: True\r\n  manual: False\r\n  description: Use optimized code paths for integer-gmp\r\n\r\n--------------------------------------------------------------------------------\r\n-- Library\r\n\r\nlibrary\r\n  default-language:  Haskell2010\r\n  ghc-options:       -Wall\r\n  include-dirs:      src/cbits\r\n  hs-source-dirs:    src\r\n\r\n  exposed-modules:\r\n    Codec.CBOR\r\n    Codec.CBOR.Decoding\r\n    Codec.CBOR.Encoding\r\n    Codec.CBOR.FlatTerm\r\n    Codec.CBOR.Magic\r\n    Codec.CBOR.Pretty\r\n    Codec.CBOR.Read\r\n    Codec.CBOR.Write\r\n    Codec.CBOR.Term\r\n    Codec.CBOR.ByteArray\r\n    Codec.CBOR.ByteArray.Sliced\r\n\r\n  other-modules:\r\n    Codec.CBOR.ByteArray.Internal\r\n\r\n  other-extensions:\r\n    CPP, ForeignFunctionInterface, MagicHash,\r\n    UnboxedTuples, BangPatterns, DeriveDataTypeable,\r\n    RankNTypes\r\n\r\n  build-depends:\r\n    array                   >= 0.4     && < 0.6,\r\n    base                    >= 4.11    && < 4.18,\r\n    bytestring              >= 0.10.4  && < 0.12,\r\n    containers              >= 0.5     && < 0.7,\r\n    deepseq                 >= 1.0     && < 1.5,\r\n    ghc-prim                >= 0.3.1.0 && < 0.10,\r\n    half                    >= 0.2.2.3 && < 0.4,\r\n    primitive               >= 0.5     && < 0.8,\r\n    text                    >= 1.1     && < 1.3 || >= 2.0 && <2.1\r\n\r\n  if flag(optimize-gmp)\r\n    cpp-options:            -DOPTIMIZE_GMP\r\n    if impl(ghc >= 9.0)\r\n      cpp-options:          -DHAVE_GHC_BIGNUM\r\n      build-depends: ghc-bignum >= 1.0 && < 2.0\r\n    else\r\n      build-depends: integer-gmp >= 1.0 && < 2.0\r\n\r\n  if impl(ghc >= 8.0)\r\n    ghc-options: -Wcompat -Wnoncanonical-monad-instances\r\n  else\r\n    build-depends:\r\n      -- provide/emulate `Control.Monad.Fail` and `Data.Semigroups` API for pre-GHC8\r\n      fail                    == 4.9.*,\r\n      semigroups              >= 0.18 && < 0.21,\r\n      -- the `PS` pattern synonym in bytestring 0.11 is unavailable with GHC < 8.0\r\n      bytestring              < 0.11\r\n\r\ntest-suite tests\r\n  type:              exitcode-stdio-1.0\r\n  hs-source-dirs:    tests\r\n  main-is:           Main.hs\r\n\r\n  default-language:  Haskell2010\r\n  ghc-options:\r\n    -Wall -fno-warn-orphans\r\n    -threaded -rtsopts \"-with-rtsopts=-N2\"\r\n\r\n  other-modules:\r\n    Tests.UnitTests\r\n    Tests.Properties\r\n    Tests.Boundary\r\n    Tests.ByteOffset\r\n    Tests.Canonical\r\n    Tests.PreEncoded\r\n    Tests.Regress\r\n    Tests.Regress.Issue160\r\n    Tests.Regress.Issue162\r\n    Tests.Regress.FlatTerm\r\n    Tests.Reference\r\n    Tests.Reference.Implementation\r\n    Tests.Reference.Generators\r\n    Tests.Reference.TestVectors\r\n    Tests.Term\r\n    Tests.UTF8\r\n    Tests.Util\r\n\r\n  build-depends:\r\n    array                   >= 0.4     && < 0.6,\r\n    base                    >= 4.11    && < 4.18,\r\n    base-orphans,\r\n    bytestring              >= 0.10.4  && < 0.12,\r\n    text                    >= 1.1     && < 2.1,\r\n    cborg,\r\n    aeson                   >= 0.7     && < 2.2,\r\n    base64-bytestring       >= 1.0     && < 1.3,\r\n    base16-bytestring       >= 1.0     && < 1.1,\r\n    deepseq                 >= 1.0     && < 1.5,\r\n    half                    >= 0.2.2.3 && < 0.4,\r\n    QuickCheck              >= 2.9     && < 2.15,\r\n    random,\r\n    scientific              >= 0.3     && < 0.4,\r\n    tasty                   >= 0.11    && < 1.5,\r\n    tasty-hunit             >= 0.9     && < 0.11,\r\n    tasty-quickcheck        >= 0.8     && < 0.11,\r\n    vector                  >= 0.10    && < 0.14\r\n  if !impl(ghc >= 8.0)\r\n    build-depends:\r\n      fail                    >= 4.9.0.0 && < 4.10\r\n";
    }