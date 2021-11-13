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
      specVersion = "1.10";
      identifier = { name = "hpc"; version = "0.6.1.0"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "ghc-devs@haskell.org";
      author = "Andy Gill";
      homepage = "";
      url = "";
      synopsis = "Code Coverage Library for Haskell";
      description = "This package provides the code coverage library for Haskell.\n\nSee <http://www.haskell.org/haskellwiki/Haskell_program_coverage> for more\ninformation.";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" ];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [ "changelog.md" ];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          ];
        buildable = true;
        modules = [
          "Trace/Hpc/Util"
          "Trace/Hpc/Mix"
          "Trace/Hpc/Tix"
          "Trace/Hpc/Reflect"
          ];
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../libraries/hpc; }