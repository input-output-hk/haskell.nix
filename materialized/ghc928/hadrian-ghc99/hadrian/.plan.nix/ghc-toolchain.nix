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
      specVersion = "2.4";
      identifier = { name = "ghc-toolchain"; version = "0.1.0.0"; };
      license = "NONE";
      copyright = "(c) The GHC Developers";
      maintainer = "ben@well-typed.com";
      author = "Ben Gamari";
      homepage = "";
      url = "";
      synopsis = "Utility for managing GHC target toolchains";
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
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."process" or (errorHandler.buildDepError "process"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."ghc-platform" or (errorHandler.buildDepError "ghc-platform"))
          ];
        buildable = true;
        modules = [
          "GHC/Toolchain"
          "GHC/Toolchain/Lens"
          "GHC/Toolchain/Monad"
          "GHC/Toolchain/PlatformDetails"
          "GHC/Toolchain/Prelude"
          "GHC/Toolchain/Program"
          "GHC/Toolchain/ParseTriple"
          "GHC/Toolchain/CheckArm"
          "GHC/Toolchain/Target"
          "GHC/Toolchain/Tools/Ar"
          "GHC/Toolchain/Tools/Cc"
          "GHC/Toolchain/Tools/Cxx"
          "GHC/Toolchain/Tools/Cpp"
          "GHC/Toolchain/Tools/Link"
          "GHC/Toolchain/Tools/Nm"
          "GHC/Toolchain/Tools/Ranlib"
          "GHC/Toolchain/Tools/Readelf"
          "GHC/Toolchain/Tools/MergeObjs"
          "GHC/Toolchain/Utils"
          ];
        hsSourceDirs = [ "src" ];
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../../utils/ghc-toolchain; }