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
      specVersion = "2.2";
      identifier = { name = "pkgb"; version = "0.1.0.0"; };
      license = "LicenseRef-PublicDomain";
      copyright = "";
      maintainer = "rodney.lorrimar@iohk.io";
      author = "Rodney Lorrimar";
      homepage = "";
      url = "";
      synopsis = "";
      description = "";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [];
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
          (hsPkgs."pkga" or (errorHandler.buildDepError "pkga"))
          (hsPkgs."conduit" or (errorHandler.buildDepError "conduit"))
          (hsPkgs."conduit-extra" or (errorHandler.buildDepError "conduit-extra"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."resourcet" or (errorHandler.buildDepError "resourcet"))
          ];
        buildable = true;
        modules = [ "ConduitExample" "PkgB" ];
        hsSourceDirs = [ "src" ];
        };
      exes = {
        "pkgb" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."pkgb" or (errorHandler.buildDepError "pkgb"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            ];
          buildable = true;
          hsSourceDirs = [ "app" ];
          mainPath = [ "Main.hs" ];
          };
        };
      tests = {
        "tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."pkgb" or (errorHandler.buildDepError "pkgb"))
            (hsPkgs."process" or (errorHandler.buildDepError "process"))
            ];
          build-tools = [
            (hsPkgs.buildPackages.pkga.components.exes.pkga-exe or (pkgs.buildPackages.pkga-exe or (errorHandler.buildToolDepError "pkga:pkga-exe")))
            ];
          buildable = true;
          hsSourceDirs = [ "app" ];
          mainPath = [ "tests.hs" ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../pkgb; }