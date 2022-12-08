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
      identifier = {
        name = "indexed-traversable-instances";
        version = "0.1.1.1";
        };
      license = "BSD-2-Clause";
      copyright = "";
      maintainer = "Oleg Grenrus <oleg.grenrus@iki.fi>";
      author = "Edward Kmett";
      homepage = "";
      url = "";
      synopsis = "More instances of FunctorWithIndex, FoldableWithIndex, TraversableWithIndex";
      description = "This package provides extra instances for type-classes in the [indexed-traversable](https://hackage.haskell.org/package/indexed-traversable) package.\n\nThe intention is to keep this package minimal;\nit provides instances that formely existed in @lens@ or @optics-extra@.\nWe recommend putting other instances directly into their defining packages.\nThe @indexed-traversable@ package is light, having only GHC boot libraries\nas its dependencies.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."indexed-traversable" or (errorHandler.buildDepError "indexed-traversable"))
          (hsPkgs."OneTuple" or (errorHandler.buildDepError "OneTuple"))
          (hsPkgs."tagged" or (errorHandler.buildDepError "tagged"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          ];
        buildable = true;
        };
      tests = {
        "safe" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."indexed-traversable" or (errorHandler.buildDepError "indexed-traversable"))
            (hsPkgs."indexed-traversable-instances" or (errorHandler.buildDepError "indexed-traversable-instances"))
            ];
          buildable = true;
          };
        "indexed-tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."indexed-traversable" or (errorHandler.buildDepError "indexed-traversable"))
            (hsPkgs."indexed-traversable-instances" or (errorHandler.buildDepError "indexed-traversable-instances"))
            (hsPkgs."OneTuple" or (errorHandler.buildDepError "OneTuple"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."quickcheck-instances" or (errorHandler.buildDepError "quickcheck-instances"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/indexed-traversable-instances-0.1.1.1.tar.gz";
      sha256 = "895ee5063d59e2b58930ca6fa54885bb11f19c8b7c7ba6c7b014f9431cdcc0b0";
      });
    }