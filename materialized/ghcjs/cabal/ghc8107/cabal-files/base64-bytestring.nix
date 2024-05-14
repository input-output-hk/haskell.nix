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
    flags = {};
    package = {
      specVersion = "1.12";
      identifier = { name = "base64-bytestring"; version = "1.2.1.0"; };
      license = "BSD-3-Clause";
      copyright = "2010-2020 Bryan O'Sullivan et al.";
      maintainer = "Herbert Valerio Riedel <hvr@gnu.org>,\nMikhail Glushenkov <mikhail.glushenkov@gmail.com>,\nEmily Pillmore <emilypi@cohomolo.gy>";
      author = "Bryan O'Sullivan <bos@serpentine.com>";
      homepage = "https://github.com/haskell/base64-bytestring";
      url = "";
      synopsis = "Fast base64 encoding and decoding for ByteStrings";
      description = "This package provides support for encoding and decoding binary data according to @base64@ (see also <https://tools.ietf.org/html/rfc4648 RFC 4648>) for strict and lazy ByteStrings\n\nFor a fuller-featured and better-performing Base64 library, see the <https://hackage.haskell.org/package/base64 base64> package.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
        ];
        buildable = true;
      };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base64-bytestring" or (errorHandler.buildDepError "base64-bytestring"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."test-framework" or (errorHandler.buildDepError "test-framework"))
            (hsPkgs."test-framework-hunit" or (errorHandler.buildDepError "test-framework-hunit"))
            (hsPkgs."test-framework-quickcheck2" or (errorHandler.buildDepError "test-framework-quickcheck2"))
          ];
          buildable = true;
        };
      };
      benchmarks = {
        "benchmarks" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base64-bytestring" or (errorHandler.buildDepError "base64-bytestring"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/base64-bytestring-1.2.1.0.tar.gz";
      sha256 = "fbf8ed30edde271eb605352021431d8f1b055f95a56af31fe2eacf6bdfdc49c9";
    });
  }) // {
    package-description-override = "cabal-version:      1.12\r\nname:               base64-bytestring\r\nversion:            1.2.1.0\r\nx-revision: 1\r\nsynopsis:           Fast base64 encoding and decoding for ByteStrings\r\ndescription:\r\n  This package provides support for encoding and decoding binary data according to @base64@ (see also <https://tools.ietf.org/html/rfc4648 RFC 4648>) for strict and lazy ByteStrings\r\n  .\r\n  For a fuller-featured and better-performing Base64 library, see the <https://hackage.haskell.org/package/base64 base64> package.\r\n\r\nhomepage:           https://github.com/haskell/base64-bytestring\r\nbug-reports:        https://github.com/haskell/base64-bytestring/issues\r\nlicense:            BSD3\r\nlicense-file:       LICENSE\r\nauthor:             Bryan O'Sullivan <bos@serpentine.com>\r\nmaintainer:\r\n  Herbert Valerio Riedel <hvr@gnu.org>,\r\n  Mikhail Glushenkov <mikhail.glushenkov@gmail.com>,\r\n  Emily Pillmore <emilypi@cohomolo.gy>\r\n\r\ncopyright:          2010-2020 Bryan O'Sullivan et al.\r\ncategory:           Data\r\nbuild-type:         Simple\r\ntested-with:\r\n  GHC ==7.0.4\r\n   || ==7.2.2\r\n   || ==7.4.2\r\n   || ==7.6.3\r\n   || ==7.8.4\r\n   || ==7.10.3\r\n   || ==8.0.2\r\n   || ==8.2.2\r\n   || ==8.4.4\r\n   || ==8.6.5\r\n   || ==8.8.4\r\n   || ==8.10.7\r\n\r\nextra-source-files:\r\n  README.md\r\n  CHANGELOG.md\r\n  utils/Transcode.hs\r\n  utils/transcode.py\r\n\r\nlibrary\r\n  exposed-modules:\r\n    Data.ByteString.Base64\r\n    Data.ByteString.Base64.Lazy\r\n    Data.ByteString.Base64.URL\r\n    Data.ByteString.Base64.URL.Lazy\r\n\r\n  other-modules:    Data.ByteString.Base64.Internal\r\n  build-depends:\r\n      base        >=4   && <5\r\n    , bytestring  >=0.9 && <0.13\r\n\r\n  ghc-options:      -Wall -funbox-strict-fields\r\n  default-language: Haskell2010\r\n\r\ntest-suite test\r\n  type:             exitcode-stdio-1.0\r\n  hs-source-dirs:   tests\r\n  main-is:          Tests.hs\r\n  ghc-options:      -Wall -threaded -rtsopts\r\n  build-depends:\r\n      base\r\n    , base64-bytestring\r\n    , bytestring\r\n    , HUnit\r\n    , QuickCheck\r\n    , test-framework\r\n    , test-framework-hunit\r\n    , test-framework-quickcheck2\r\n\r\n  default-language: Haskell2010\r\n\r\nbenchmark benchmarks\r\n  type:             exitcode-stdio-1.0\r\n  hs-source-dirs:   benchmarks\r\n  main-is:          BM.hs\r\n  ghc-options:      -Wall -threaded -rtsopts\r\n  build-depends:\r\n      base\r\n    , base64-bytestring\r\n    , bytestring\r\n    , criterion\r\n    , deepseq >=1.1\r\n\r\n  default-language: Haskell2010\r\n\r\nsource-repository head\r\n  type:     git\r\n  location: git://github.com/haskell/base64-bytestring\r\n";
  }