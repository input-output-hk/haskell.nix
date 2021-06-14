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
      identifier = { name = "pkga"; version = "0.1.0.0"; };
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
          (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          ];
        buildable = true;
        modules = [ "PkgA" ];
        };
      exes = {
        "pkga-exe" = {
          depends = [ (hsPkgs."base" or (errorHandler.buildDepError "base")) ];
          buildable = true;
          hsSourceDirs = [ "." ];
          mainPath = [ "MainA.hs" ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../pkga; }