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
    flags = {
      boundschecks = true;
      unsafechecks = false;
      internalchecks = false;
      wall = false;
      };
    package = {
      specVersion = "1.10";
      identifier = { name = "vector"; version = "0.12.2.0"; };
      license = "BSD-3-Clause";
      copyright = "(c) Roman Leshchinskiy 2008-2012";
      maintainer = "Haskell Libraries Team <libraries@haskell.org>";
      author = "Roman Leshchinskiy <rl@cse.unsw.edu.au>";
      homepage = "https://github.com/haskell/vector";
      url = "";
      synopsis = "Efficient Arrays";
      description = "\nAn efficient implementation of Int-indexed arrays (both mutable\nand immutable), with a powerful loop optimisation framework .\n\nIt is structured as follows:\n\n[\"Data.Vector\"] Boxed vectors of arbitrary types.\n\n[\"Data.Vector.Unboxed\"] Unboxed vectors with an adaptive\nrepresentation based on data type families.\n\n[\"Data.Vector.Storable\"] Unboxed vectors of 'Storable' types.\n\n[\"Data.Vector.Primitive\"] Unboxed vectors of primitive types as\ndefined by the @primitive@ package. \"Data.Vector.Unboxed\" is more\nflexible at no performance cost.\n\n[\"Data.Vector.Generic\"] Generic interface to the vector types.\n\nThere is also a (draft) tutorial on common uses of vector.\n\n* <http://haskell.org/haskellwiki/Numeric_Haskell:_A_Vector_Tutorial>";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
          (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          ] ++ (pkgs.lib).optionals (!(compiler.isGhc && (compiler.version).gt "8.0")) [
          (hsPkgs."fail" or (errorHandler.buildDepError "fail"))
          (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
          ];
        buildable = true;
        };
      tests = {
        "vector-tests-O0" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
            (hsPkgs."base-orphans" or (errorHandler.buildDepError "base-orphans"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            ] ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).gt "8.0")) (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"));
          buildable = true;
          };
        "vector-tests-O2" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
            (hsPkgs."base-orphans" or (errorHandler.buildDepError "base-orphans"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            ] ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).gt "8.0")) (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"));
          buildable = true;
          };
        "vector-doctest" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."doctest" or (errorHandler.buildDepError "doctest"))
            (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            ];
          buildable = (if compiler.isGhc && (compiler.version).lt "8.6"
            then false
            else true) && (if compiler.isGhc && (compiler.version).ge "8.10" && (compiler.isGhc && (compiler.version).lt "8.11")
            then false
            else true);
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/vector-0.12.2.0.tar.gz";
      sha256 = "17ab0b84c87859333ff681bb9f768368779677925bd589ff4baa05be3fd26b50";
      });
    }