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
      };
    };
  } // rec { src = pkgs.lib.mkDefault ./.; }
