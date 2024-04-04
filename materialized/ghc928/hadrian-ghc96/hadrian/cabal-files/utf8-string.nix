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
      specVersion = "1.10";
      identifier = { name = "utf8-string"; version = "1.0.2"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "emertens@galois.com";
      author = "Eric Mertens";
      homepage = "https://github.com/glguy/utf8-string/";
      url = "";
      synopsis = "Support for reading and writing UTF8 Strings";
      description = "A UTF8 layer for Strings. The utf8-string\npackage provides operations for encoding UTF8\nstrings to Word8 lists and back, and for reading and\nwriting UTF8 without truncation.";
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
        "unit-tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."utf8-string" or (errorHandler.buildDepError "utf8-string"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/utf8-string-1.0.2.tar.gz";
      sha256 = "ee48deada7600370728c4156cb002441de770d0121ae33a68139a9ed9c19b09a";
      });
    }) // {
    package-description-override = "Name:               utf8-string\nVersion:            1.0.2\nAuthor:             Eric Mertens\nMaintainer:         emertens@galois.com\nLicense:            BSD3\nLicense-file:       LICENSE\nHomepage:           https://github.com/glguy/utf8-string/\nBug-Reports:        https://github.com/glguy/utf8-string/issues\nSynopsis:           Support for reading and writing UTF8 Strings\nDescription:        A UTF8 layer for Strings. The utf8-string\n                    package provides operations for encoding UTF8\n                    strings to Word8 lists and back, and for reading and\n                    writing UTF8 without truncation.\nCategory:           Codec\nBuild-type:         Simple\ncabal-version:      >= 1.10\nExtra-Source-Files: CHANGELOG.markdown\nTested-With:        GHC==7.0.4, GHC==7.4.2, GHC==7.6.3, GHC==7.8.4, GHC==7.10.3, GHC==8.0.2, GHC==8.2.1\n\nsource-repository head\n  type:               git\n  location:           https://github.com/glguy/utf8-string\n\nlibrary\n  Ghc-options:        -W -O2\n\n  build-depends:      base >= 4.3 && < 5, bytestring >= 0.9\n\n  Exposed-modules:    Codec.Binary.UTF8.String\n                      Codec.Binary.UTF8.Generic\n                      Data.String.UTF8\n                      Data.ByteString.UTF8\n                      Data.ByteString.Lazy.UTF8\n\n  default-language:   Haskell2010\n\ntest-suite unit-tests\n  type:               exitcode-stdio-1.0\n  hs-source-dirs:     tests\n  main-is:            Tests.hs\n  build-depends:      base, HUnit >= 1.3 && < 1.7, utf8-string\n  default-language:   Haskell2010\n";
    }