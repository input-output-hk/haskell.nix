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
    flags = { force-o2 = false; };
    package = {
      specVersion = "1.12";
      identifier = { name = "regex-tdfa"; version = "1.3.2"; };
      license = "BSD-3-Clause";
      copyright = "Copyright (c) 2007-2009, Christopher Kuklewicz";
      maintainer = "Andreas Abel";
      author = "Christopher Kuklewicz";
      homepage = "https://wiki.haskell.org/Regular_expressions";
      url = "";
      synopsis = "Pure Haskell Tagged DFA Backend for \"Text.Regex\" (regex-base)";
      description = "This package provides a pure Haskell \\\"Tagged\\\" DFA regex engine for <//hackage.haskell.org/package/regex-base regex-base>. This implementation was inspired by the algorithm (and Master's thesis) behind the regular expression library known as <https://github.com/laurikari/tre/ TRE or libtre>.\n\nPlease consult the \"Text.Regex.TDFA\" module for API documentation including a tutorial with usage examples;\nsee also <https://wiki.haskell.org/Regular_expressions> for general information about regular expression support in Haskell.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."parsec" or (errorHandler.buildDepError "parsec"))
          (hsPkgs."regex-base" or (errorHandler.buildDepError "regex-base"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          ] ++ (pkgs.lib).optionals (!(compiler.isGhc && (compiler.version).ge "8.0")) [
          (hsPkgs."fail" or (errorHandler.buildDepError "fail"))
          (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
          ];
        buildable = true;
        };
      tests = {
        "regex-tdfa-unittest" = {
          depends = [
            (hsPkgs."regex-tdfa" or (errorHandler.buildDepError "regex-tdfa"))
            (hsPkgs."array" or (errorHandler.buildDepError "array"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."regex-base" or (errorHandler.buildDepError "regex-base"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."utf8-string" or (errorHandler.buildDepError "utf8-string"))
            ] ++ (pkgs.lib).optionals (!(compiler.isGhc && (compiler.version).ge "8.0")) [
            (hsPkgs."fail" or (errorHandler.buildDepError "fail"))
            (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
            ];
          buildable = true;
          };
        "doc-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."regex-tdfa" or (errorHandler.buildDepError "regex-tdfa"))
            (hsPkgs."doctest-parallel" or (errorHandler.buildDepError "doctest-parallel"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/regex-tdfa-1.3.2.tar.gz";
      sha256 = "2660d7dc5f00718c39454c844d354dea26d605db9864f80951efad5f7284dfd1";
      });
    }