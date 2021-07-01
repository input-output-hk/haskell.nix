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
    flags = { install-examples = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "hackage-db"; version = "2.1.0.1"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Peter Simons <simons@cryp.to>";
      author = "Peter Simons, Alexander Altman, Ben James";
      homepage = "https://github.com/peti/hackage-db#readme";
      url = "";
      synopsis = "Access cabal-install's Hackage database via Data.Map";
      description = "This library provides convenient access to the local copy of the Hackage\ndatabase that \\\"cabal update\\\" creates. Check out\n<https://github.com/peti/hackage-db/tree/master/example/> for a collection\nof simple example programs that demonstrate how to use this code.";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" ];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."Cabal" or (errorHandler.buildDepError "Cabal"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."tar" or (errorHandler.buildDepError "tar"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."utf8-string" or (errorHandler.buildDepError "utf8-string"))
          ];
        buildable = true;
        modules = [
          "Paths_hackage_db"
          "Distribution/Hackage/DB"
          "Distribution/Hackage/DB/Builder"
          "Distribution/Hackage/DB/Errors"
          "Distribution/Hackage/DB/MetaData"
          "Distribution/Hackage/DB/Parsed"
          "Distribution/Hackage/DB/Path"
          "Distribution/Hackage/DB/Unparsed"
          "Distribution/Hackage/DB/Utility"
          ];
        hsSourceDirs = [ "src" ];
        };
      exes = {
        "list-known-versions" = {
          depends = (pkgs.lib).optionals (flags.install-examples) [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."Cabal" or (errorHandler.buildDepError "Cabal"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."hackage-db" or (errorHandler.buildDepError "hackage-db"))
            ];
          buildable = if flags.install-examples then true else false;
          hsSourceDirs = [ "example" ];
          mainPath = [ "list-known-versions.hs" ] ++ [ "" ];
          };
        "show-meta-data" = {
          depends = (pkgs.lib).optionals (flags.install-examples) [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."Cabal" or (errorHandler.buildDepError "Cabal"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."hackage-db" or (errorHandler.buildDepError "hackage-db"))
            (hsPkgs."utf8-string" or (errorHandler.buildDepError "utf8-string"))
            ];
          buildable = if flags.install-examples then true else false;
          hsSourceDirs = [ "example" ];
          mainPath = [ "show-meta-data.hs" ] ++ [ "" ];
          };
        "show-package-versions" = {
          depends = (pkgs.lib).optionals (flags.install-examples) [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."Cabal" or (errorHandler.buildDepError "Cabal"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."hackage-db" or (errorHandler.buildDepError "hackage-db"))
            ];
          buildable = if flags.install-examples then true else false;
          hsSourceDirs = [ "example" ];
          mainPath = [ "show-package-versions.hs" ] ++ [ "" ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault .././.source-repository-packages/0; }