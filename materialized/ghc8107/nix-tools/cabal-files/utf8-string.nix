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
    }