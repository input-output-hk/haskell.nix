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
    flags = { integer-simple = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "bytestring"; version = "0.10.10.0"; };
      license = "BSD-3-Clause";
      copyright = "Copyright (c) Don Stewart          2005-2009,\n(c) Duncan Coutts        2006-2015,\n(c) David Roundy         2003-2005,\n(c) Jasper Van der Jeugt 2010,\n(c) Simon Meier          2010-2013.";
      maintainer = "Duncan Coutts <duncan@community.haskell.org>";
      author = "Don Stewart,\nDuncan Coutts";
      homepage = "https://github.com/haskell/bytestring";
      url = "";
      synopsis = "Fast, compact, strict and lazy byte strings with a list interface";
      description = "An efficient compact, immutable byte string type (both strict and lazy)\nsuitable for binary or 8-bit character data.\n\nThe 'ByteString' type represents sequences of bytes or 8-bit characters.\nIt is suitable for high performance use, both in terms of large data\nquantities, or high speed requirements. The 'ByteString' functions follow\nthe same style as Haskell\\'s ordinary lists, so it is easy to convert code\nfrom using 'String' to 'ByteString'.\n\nTwo 'ByteString' variants are provided:\n\n* Strict 'ByteString's keep the string as a single large array. This\nmakes them convenient for passing data between C and Haskell.\n\n* Lazy 'ByteString's use a lazy list of strict chunks which makes it\nsuitable for I\\/O streaming tasks.\n\nThe @Char8@ modules provide a character-based view of the same\nunderlying 'ByteString' types. This makes it convenient to handle mixed\nbinary and 8-bit character content (which is common in many file formats\nand network protocols).\n\nThe 'Builder' module provides an efficient way to build up 'ByteString's\nin an ad-hoc way by repeated concatenation. This is ideal for fast\nserialisation or pretty printing.\n\nThere is also a 'ShortByteString' type which has a lower memory overhead\nand can can be converted to or from a 'ByteString', but supports very few\nother operations. It is suitable for keeping many short strings in memory.\n\n'ByteString's are not designed for Unicode. For Unicode strings you should\nuse the 'Text' type from the @text@ package.\n\nThese modules are intended to be imported qualified, to avoid name clashes\nwith \"Prelude\" functions, e.g.\n\n> import qualified Data.ByteString as BS";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = ([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          ] ++ (pkgs.lib).optionals (compiler.isGhc && (compiler.version).ge "6.11") ((pkgs.lib).optional (!flags.integer-simple) (hsPkgs."integer-gmp" or (errorHandler.buildDepError "integer-gmp")))) ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).ge "6.9" && (compiler.isGhc && (compiler.version).lt "6.11")) (hsPkgs."integer" or (errorHandler.buildDepError "integer"));
        buildable = true;
        };
      tests = {
        "prop-compiled" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."test-framework" or (errorHandler.buildDepError "test-framework"))
            (hsPkgs."test-framework-quickcheck2" or (errorHandler.buildDepError "test-framework-quickcheck2"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            ];
          buildable = true;
          };
        "regressions" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."test-framework" or (errorHandler.buildDepError "test-framework"))
            (hsPkgs."test-framework-hunit" or (errorHandler.buildDepError "test-framework-hunit"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            ];
          buildable = false;
          };
        "test-builder" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."byteorder" or (errorHandler.buildDepError "byteorder"))
            (hsPkgs."dlist" or (errorHandler.buildDepError "dlist"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."test-framework" or (errorHandler.buildDepError "test-framework"))
            (hsPkgs."test-framework-hunit" or (errorHandler.buildDepError "test-framework-hunit"))
            (hsPkgs."test-framework-quickcheck2" or (errorHandler.buildDepError "test-framework-quickcheck2"))
            ];
          buildable = true;
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ./.; }
