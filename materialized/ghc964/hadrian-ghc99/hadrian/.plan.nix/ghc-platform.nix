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
      specVersion = "3.0";
      identifier = { name = "ghc-platform"; version = "0.1.0.0"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "ghc-devs@haskell.org";
      author = "Rodrigo Mesquita";
      homepage = "";
      url = "";
      synopsis = "Platform information used by GHC and friends";
      description = "";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" ];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [];
      extraTmpFiles = [];
      extraDocFiles = [ "CHANGELOG.md" ];
    };
    components = {
      "library" = {
        depends = [ (hsPkgs."base" or (errorHandler.buildDepError "base")) ];
        buildable = true;
        modules = [ "GHC/Platform/ArchOS" ];
        hsSourceDirs = [ "src" ];
      };
    };
  } // rec { src = pkgs.lib.mkDefault ../../libraries/ghc-platform; }