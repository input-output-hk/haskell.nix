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
      specVersion = "1.12";
      identifier = { name = "unliftio-core"; version = "0.2.0.1"; };
      license = "MIT";
      copyright = "2017-2020 FP Complete";
      maintainer = "michael@snoyman.com";
      author = "Michael Snoyman, Francesco Mazzoli";
      homepage = "https://github.com/fpco/unliftio/tree/master/unliftio-core#readme";
      url = "";
      synopsis = "The MonadUnliftIO typeclass for unlifting monads to IO";
      description = "Please see the documentation and README at <https://www.stackage.org/package/unliftio-core>";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/unliftio-core-0.2.0.1.tar.gz";
      sha256 = "919f0d1297ea2f5373118553c1df2a9405d8b9e31a8307e829da67d4953c299a";
      });
    }