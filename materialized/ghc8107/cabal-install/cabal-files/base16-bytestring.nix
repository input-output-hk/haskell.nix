{ system
  , compiler
  , flags
  , pkgs
  , hsPkgs
  , pkgconfPkgs
  , errorHandler
  , config
  , ... }:
  {
    flags = {};
    package = {
      specVersion = "1.12";
      identifier = { name = "base16-bytestring"; version = "1.0.2.0"; };
      license = "BSD-3-Clause";
      copyright = "Copyright 2011 MailRank, Inc.;\nCopyright 2010-2020 Bryan O'Sullivan et al.";
      maintainer = "Herbert Valerio Riedel <hvr@gnu.org>,\nMikhail Glushenkov <mikhail.glushenkov@gmail.com>,\nEmily Pillmore <emilypi@cohomolo.gy>";
      author = "Bryan O'Sullivan <bos@serpentine.com>";
      homepage = "http://github.com/haskell/base16-bytestring";
      url = "";
      synopsis = "RFC 4648-compliant Base16 encodings for ByteStrings";
      description = "This package provides support for encoding and decoding binary data according\nto @base16@ (see also <https://tools.ietf.org/html/rfc4648 RFC 4648>) for\nstrict (see \"Data.ByteString.Base16\") and lazy @ByteString@s (see \"Data.ByteString.Base16.Lazy\").\n\nSee the <https://hackage.haskell.org/package/base16 base16> package which provides superior encoding and decoding performance as well as support for lazy, short, and strict variants of 'Text' and 'ByteString' values. Additionally, see the <https://hackage.haskell.org/package/base-encoding base-encoding> package which\nprovides an uniform API providing conversion paths between more binary and textual types.";
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
            (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
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
        "bench" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/base16-bytestring-1.0.2.0.tar.gz";
      sha256 = "1d5a91143ef0e22157536093ec8e59d226a68220ec89378d5dcaeea86472c784";
      });
    }