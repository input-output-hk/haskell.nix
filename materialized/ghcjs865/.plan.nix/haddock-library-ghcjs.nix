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
      specVersion = "2.0";
      identifier = { name = "haddock-library-ghcjs"; version = "1.6.0"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Alex Biehl <alexbiehl@gmail.com>, Simon Hengel <sol@typeful.net>, Mateusz Kowalczyk <fuuzetsu@fuuzetsu.co.uk>";
      author = "";
      homepage = "http://www.haskell.org/haddock/";
      url = "";
      synopsis = "Library exposing some functionality of Haddock.";
      description = "Haddock is a documentation-generation tool for Haskell\nlibraries. These modules expose some functionality of it\nwithout pulling in the GHC dependency. Please note that the\nAPI is likely to change so specify upper bounds in your\nproject if you can't release often. For interacting with Haddock\nitself, see the ‘haddock’ package.";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" ];
      dataDir = "";
      dataFiles = [];
      extraSrcFiles = [ "CHANGES.md" ];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."parsec" or (errorHandler.buildDepError "parsec"))
          ];
        buildable = true;
        modules = [
          "Documentation/Haddock/Parser/Util"
          "Documentation/Haddock/Doc"
          "Documentation/Haddock/Markup"
          "Documentation/Haddock/Parser"
          "Documentation/Haddock/Parser/Monad"
          "Documentation/Haddock/Types"
          "Documentation/Haddock/Utf8"
          ];
        hsSourceDirs = [ "src" ];
        };
      tests = {
        "spec" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base-compat" or (errorHandler.buildDepError "base-compat"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."parsec" or (errorHandler.buildDepError "parsec"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            ];
          build-tools = [
            (hsPkgs.buildPackages.hspec-discover or (pkgs.buildPackages.hspec-discover or (errorHandler.buildToolDepError "hspec-discover")))
            ];
          buildable = true;
          modules = [
            "Documentation/Haddock/Doc"
            "Documentation/Haddock/Parser"
            "Documentation/Haddock/Parser/Monad"
            "Documentation/Haddock/Parser/Util"
            "Documentation/Haddock/Parser/UtilSpec"
            "Documentation/Haddock/ParserSpec"
            "Documentation/Haddock/Types"
            "Documentation/Haddock/Utf8"
            "Documentation/Haddock/Utf8Spec"
            ];
          hsSourceDirs = [ "test" "src" ];
          mainPath = [ "Spec.hs" ];
          };
        "fixtures" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base-compat" or (errorHandler.buildDepError "base-compat"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."tree-diff" or (errorHandler.buildDepError "tree-diff"))
            (hsPkgs."haddock-library" or (errorHandler.buildDepError "haddock-library"))
            ];
          buildable = true;
          hsSourceDirs = [ "fixtures" ];
          mainPath = [ "Fixtures.hs" ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../lib/haddock-library-ghcjs; }