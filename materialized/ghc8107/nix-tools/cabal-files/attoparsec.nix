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
    flags = { developer = false; };
    package = {
      specVersion = "2.0";
      identifier = { name = "attoparsec"; version = "0.14.4"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Bryan O'Sullivan <bos@serpentine.com>, Ben Gamari <ben@smart-cactus.org>";
      author = "Bryan O'Sullivan <bos@serpentine.com>";
      homepage = "https://github.com/bgamari/attoparsec";
      url = "";
      synopsis = "Fast combinator parsing for bytestrings and text";
      description = "A fast parser combinator library, aimed particularly at dealing\nefficiently with network protocols and complicated text/binary\nfile formats.";
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
          (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
          (hsPkgs."attoparsec".components.sublibs.attoparsec-internal or (errorHandler.buildDepError "attoparsec:attoparsec-internal"))
          ] ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).lt "7.4") (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))) ++ (pkgs.lib).optionals (!(compiler.isGhc && (compiler.version).ge "8.0")) [
          (hsPkgs."fail" or (errorHandler.buildDepError "fail"))
          (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
          ];
        buildable = true;
        };
      sublibs = {
        "attoparsec-internal" = {
          depends = [
            (hsPkgs."array" or (errorHandler.buildDepError "array"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            ] ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8.0")) (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"));
          buildable = true;
          };
        };
      tests = {
        "attoparsec-tests" = {
          depends = [
            (hsPkgs."array" or (errorHandler.buildDepError "array"))
            (hsPkgs."attoparsec" or (errorHandler.buildDepError "attoparsec"))
            (hsPkgs."attoparsec".components.sublibs.attoparsec-internal or (errorHandler.buildDepError "attoparsec:attoparsec-internal"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."quickcheck-unicode" or (errorHandler.buildDepError "quickcheck-unicode"))
            (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            ] ++ (pkgs.lib).optionals (!(compiler.isGhc && (compiler.version).ge "8.0")) [
            (hsPkgs."fail" or (errorHandler.buildDepError "fail"))
            (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
            ];
          buildable = true;
          };
        };
      benchmarks = {
        "attoparsec-benchmarks" = {
          depends = [
            (hsPkgs."array" or (errorHandler.buildDepError "array"))
            (hsPkgs."attoparsec" or (errorHandler.buildDepError "attoparsec"))
            (hsPkgs."attoparsec".components.sublibs.attoparsec-internal or (errorHandler.buildDepError "attoparsec:attoparsec-internal"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."case-insensitive" or (errorHandler.buildDepError "case-insensitive"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
            (hsPkgs."http-types" or (errorHandler.buildDepError "http-types"))
            (hsPkgs."parsec" or (errorHandler.buildDepError "parsec"))
            (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
            (hsPkgs."tasty-bench" or (errorHandler.buildDepError "tasty-bench"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            ] ++ (pkgs.lib).optionals (!(compiler.isGhc && (compiler.version).ge "8.0")) [
            (hsPkgs."fail" or (errorHandler.buildDepError "fail"))
            (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/attoparsec-0.14.4.tar.gz";
      sha256 = "3f337fe58624565de12426f607c23e60c7b09c86b4e3adfc827ca188c9979e6c";
      });
    }