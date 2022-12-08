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
      identifier = { name = "optparse-applicative"; version = "0.15.1.0"; };
      license = "BSD-3-Clause";
      copyright = "(c) 2012-2017 Paolo Capriotti <paolo@capriotti.io>";
      maintainer = "huw.campbell@gmail.com";
      author = "Paolo Capriotti, Huw Campbell";
      homepage = "https://github.com/pcapriotti/optparse-applicative";
      url = "";
      synopsis = "Utilities and combinators for parsing command line options";
      description = "optparse-applicative is a haskell library for parsing options\non the command line, providing a powerful applicative interface\nfor composing these options.\n\noptparse-applicative takes care of reading and validating the\narguments passed to the command line, handling and reporting\nerrors, generating a usage line, a comprehensive help screen,\nand enabling context-sensitive bash completions.\n\nSee the included README for detailed instructions and examples,\nwhich is also available on github\n<https://github.com/pcapriotti/optparse-applicative>.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."transformers-compat" or (errorHandler.buildDepError "transformers-compat"))
          (hsPkgs."process" or (errorHandler.buildDepError "process"))
          (hsPkgs."ansi-wl-pprint" or (errorHandler.buildDepError "ansi-wl-pprint"))
          ] ++ (pkgs.lib).optionals (!(compiler.isGhc && (compiler.version).ge "8")) [
          (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
          (hsPkgs."fail" or (errorHandler.buildDepError "fail"))
          ];
        buildable = true;
        };
      tests = {
        "tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            ] ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8")) (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"));
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/optparse-applicative-0.15.1.0.tar.gz";
      sha256 = "4db3675fd1e0594afdf079db46f4cd412d483835d703e7c07e1a1a37d6f046f3";
      });
    }