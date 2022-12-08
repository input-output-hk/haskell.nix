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
      specVersion = "1.8";
      identifier = { name = "case-insensitive"; version = "1.2.1.0"; };
      license = "BSD-3-Clause";
      copyright = "2011 Bas van Dijk";
      maintainer = "Bas van Dijk <v.dijk.bas@gmail.com>";
      author = "Bas van Dijk";
      homepage = "https://github.com/basvandijk/case-insensitive";
      url = "";
      synopsis = "Case insensitive string comparison";
      description = "The module @Data.CaseInsensitive@ provides the 'CI' type\nconstructor which can be parameterised by a string-like\ntype like: 'String', 'ByteString', 'Text',\netc.. Comparisons of values of the resulting type will be\ninsensitive to cases.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          ] ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8.0")) (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"));
        buildable = true;
        };
      tests = {
        "test-case-insensitive" = {
          depends = [
            (hsPkgs."case-insensitive" or (errorHandler.buildDepError "case-insensitive"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."test-framework" or (errorHandler.buildDepError "test-framework"))
            (hsPkgs."test-framework-hunit" or (errorHandler.buildDepError "test-framework-hunit"))
            ];
          buildable = true;
          };
        };
      benchmarks = {
        "bench-case-insensitive" = {
          depends = [
            (hsPkgs."case-insensitive" or (errorHandler.buildDepError "case-insensitive"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
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
      url = "http://hackage.haskell.org/package/case-insensitive-1.2.1.0.tar.gz";
      sha256 = "296dc17e0c5f3dfb3d82ced83e4c9c44c338ecde749b278b6eae512f1d04e406";
      });
    }