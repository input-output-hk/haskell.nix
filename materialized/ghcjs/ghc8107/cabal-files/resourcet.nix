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
      identifier = { name = "resourcet"; version = "1.2.4.2"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "michael@snoyman.com";
      author = "Michael Snoyman";
      homepage = "http://github.com/snoyberg/conduit";
      url = "";
      synopsis = "Deterministic allocation and freeing of scarce resources.";
      description = "Hackage documentation generation is not reliable. For up to date documentation, please see: <http://www.stackage.org/package/resourcet>.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
          (hsPkgs."unliftio-core" or (errorHandler.buildDepError "unliftio-core"))
          (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
          ];
        buildable = true;
        };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."resourcet" or (errorHandler.buildDepError "resourcet"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/resourcet-1.2.4.2.tar.gz";
      sha256 = "17f20842043ad199961a801b6efb1233b9098eb3537f8395844268f6a223eb87";
      });
    }