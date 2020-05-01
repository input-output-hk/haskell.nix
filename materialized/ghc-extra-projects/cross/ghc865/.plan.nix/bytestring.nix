let
  buildDepError = pkg:
    builtins.throw ''
      The Haskell package set does not contain the package: ${pkg} (build dependency).
      
      If you are using Stackage, make sure that you are using a snapshot that contains the package. Otherwise you may need to update the Hackage snapshot you are using, usually by updating haskell.nix.
      '';
  sysDepError = pkg:
    builtins.throw ''
      The Nixpkgs package set does not contain the package: ${pkg} (system dependency).
      
      You may need to augment the system package mapping in haskell.nix so that it can be found.
      '';
  pkgConfDepError = pkg:
    builtins.throw ''
      The pkg-conf packages does not contain the package: ${pkg} (pkg-conf dependency).
      
      You may need to augment the pkg-conf package mapping in haskell.nix so that it can be found.
      '';
  exeDepError = pkg:
    builtins.throw ''
      The local executable components do not include the component: ${pkg} (executable dependency).
      '';
  legacyExeDepError = pkg:
    builtins.throw ''
      The Haskell package set does not contain the package: ${pkg} (executable dependency).
      
      If you are using Stackage, make sure that you are using a snapshot that contains the package. Otherwise you may need to update the Hackage snapshot you are using, usually by updating haskell.nix.
      '';
  buildToolDepError = pkg:
    builtins.throw ''
      Neither the Haskell package set or the Nixpkgs package set contain the package: ${pkg} (build tool dependency).
      
      If this is a system dependency:
      You may need to augment the system package mapping in haskell.nix so that it can be found.
      
      If this is a Haskell dependency:
      If you are using Stackage, make sure that you are using a snapshot that contains the package. Otherwise you may need to update the Hackage snapshot you are using, usually by updating haskell.nix.
      '';
in { system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = { integer-simple = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "bytestring"; version = "0.10.8.2"; };
      license = "BSD-3-Clause";
      copyright = "Copyright (c) Don Stewart          2005-2009,\n(c) Duncan Coutts        2006-2015,\n(c) David Roundy         2003-2005,\n(c) Jasper Van der Jeugt 2010,\n(c) Simon Meier          2010-2013.";
      maintainer = "Duncan Coutts <duncan@community.haskell.org>";
      author = "Don Stewart,\nDuncan Coutts";
      homepage = "https://github.com/haskell/bytestring";
      url = "";
      synopsis = "Fast, compact, strict and lazy byte strings with a list interface";
      description = "An efficient compact, immutable byte string type (both strict and lazy)\nsuitable for binary or 8-bit character data.\n\nThe 'ByteString' type represents sequences of bytes or 8-bit characters.\nIt is suitable for high performance use, both in terms of large data\nquantities, or high speed requirements. The 'ByteString' functions follow\nthe same style as Haskell\\'s ordinary lists, so it is easy to convert code\nfrom using 'String' to 'ByteString'.\n\nTwo 'ByteString' variants are provided:\n\n* Strict 'ByteString's keep the string as a single large array. This\nmakes them convenient for passing data between C and Haskell.\n\n* Lazy 'ByteString's use a lazy list of strict chunks which makes it\nsuitable for I\\/O streaming tasks.\n\nThe @Char8@ modules provide a character-based view of the same\nunderlying 'ByteString' types. This makes it convenient to handle mixed\nbinary and 8-bit character content (which is common in many file formats\nand network protocols).\n\nThe 'Builder' module provides an efficient way to build up 'ByteString's\nin an ad-hoc way by repeated concatenation. This is ideal for fast\nserialisation or pretty printing.\n\nThere is also a 'ShortByteString' type which has a lower memory overhead\nand can can be converted to or from a 'ByteString', but supports very few\nother operations. It is suitable for keeping many short strings in memory.\n\n'ByteString's are not designed for Unicode. For Unicode strings you should\nuse the 'Text' type from the @text@ package.\n\nThese modules are intended to be imported qualified, to avoid name clashes\nwith \"Prelude\" functions, e.g.\n\n> import qualified Data.ByteString as BS";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" ];
      dataDir = "";
      dataFiles = [];
      extraSrcFiles = [ "README.md" "Changelog.md" ];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = ([
          (hsPkgs."base" or (buildDepError "base"))
          (hsPkgs."ghc-prim" or (buildDepError "ghc-prim"))
          (hsPkgs."deepseq" or (buildDepError "deepseq"))
          ] ++ (pkgs.lib).optionals (compiler.isGhc && (compiler.version).ge "6.11") ((pkgs.lib).optional (!flags.integer-simple) (hsPkgs."integer-gmp" or (buildDepError "integer-gmp")))) ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).ge "6.9" && (compiler.isGhc && (compiler.version).lt "6.11")) (hsPkgs."integer" or (buildDepError "integer"));
        buildable = true;
        modules = [
          "Data/ByteString/Builder/ASCII"
          "Data/ByteString/Builder/Prim/Binary"
          "Data/ByteString/Builder/Prim/ASCII"
          "Data/ByteString/Builder/Prim/Internal/Floating"
          "Data/ByteString/Builder/Prim/Internal/UncheckedShifts"
          "Data/ByteString/Builder/Prim/Internal/Base16"
          "Data/ByteString"
          "Data/ByteString/Char8"
          "Data/ByteString/Unsafe"
          "Data/ByteString/Internal"
          "Data/ByteString/Lazy"
          "Data/ByteString/Lazy/Char8"
          "Data/ByteString/Lazy/Internal"
          "Data/ByteString/Short"
          "Data/ByteString/Short/Internal"
          "Data/ByteString/Builder"
          "Data/ByteString/Builder/Extra"
          "Data/ByteString/Builder/Prim"
          "Data/ByteString/Builder/Internal"
          "Data/ByteString/Builder/Prim/Internal"
          "Data/ByteString/Lazy/Builder"
          "Data/ByteString/Lazy/Builder/Extras"
          "Data/ByteString/Lazy/Builder/ASCII"
          ];
        cSources = [ "cbits/fpstring.c" "cbits/itoa.c" ];
        includeDirs = [ "include" ];
        includes = [ "fpstring.h" ];
        };
      tests = {
        "prop-compiled" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."ghc-prim" or (buildDepError "ghc-prim"))
            (hsPkgs."deepseq" or (buildDepError "deepseq"))
            (hsPkgs."random" or (buildDepError "random"))
            (hsPkgs."directory" or (buildDepError "directory"))
            (hsPkgs."test-framework" or (buildDepError "test-framework"))
            (hsPkgs."test-framework-quickcheck2" or (buildDepError "test-framework-quickcheck2"))
            (hsPkgs."QuickCheck" or (buildDepError "QuickCheck"))
            ];
          buildable = true;
          modules = [ "Rules" "QuickCheckUtils" "TestFramework" ];
          cSources = [ "cbits/fpstring.c" ];
          hsSourceDirs = [ "." "tests" ];
          includeDirs = [ "include" ];
          mainPath = [ "Properties.hs" ];
          };
        "regressions" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."ghc-prim" or (buildDepError "ghc-prim"))
            (hsPkgs."deepseq" or (buildDepError "deepseq"))
            (hsPkgs."random" or (buildDepError "random"))
            (hsPkgs."directory" or (buildDepError "directory"))
            (hsPkgs."test-framework" or (buildDepError "test-framework"))
            (hsPkgs."test-framework-hunit" or (buildDepError "test-framework-hunit"))
            (hsPkgs."HUnit" or (buildDepError "HUnit"))
            ];
          buildable = false;
          cSources = [ "cbits/fpstring.c" ];
          hsSourceDirs = [ "." "tests" ];
          includeDirs = [ "include" ];
          mainPath = [ "Regressions.hs" ];
          };
        "test-builder" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."ghc-prim" or (buildDepError "ghc-prim"))
            (hsPkgs."deepseq" or (buildDepError "deepseq"))
            (hsPkgs."QuickCheck" or (buildDepError "QuickCheck"))
            (hsPkgs."byteorder" or (buildDepError "byteorder"))
            (hsPkgs."dlist" or (buildDepError "dlist"))
            (hsPkgs."directory" or (buildDepError "directory"))
            (hsPkgs."mtl" or (buildDepError "mtl"))
            (hsPkgs."HUnit" or (buildDepError "HUnit"))
            (hsPkgs."test-framework" or (buildDepError "test-framework"))
            (hsPkgs."test-framework-hunit" or (buildDepError "test-framework-hunit"))
            (hsPkgs."test-framework-quickcheck2" or (buildDepError "test-framework-quickcheck2"))
            ];
          buildable = true;
          modules = [
            "Data/ByteString/Builder/Tests"
            "Data/ByteString/Builder/Prim/Tests"
            "Data/ByteString/Builder/Prim/TestUtils"
            "TestFramework"
            ];
          cSources = [ "cbits/fpstring.c" "cbits/itoa.c" ];
          hsSourceDirs = [ "." "tests" "tests/builder" ];
          includeDirs = [ "include" ];
          includes = [ "fpstring.h" ];
          mainPath = [ "TestSuite.hs" ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../libraries/bytestring; }