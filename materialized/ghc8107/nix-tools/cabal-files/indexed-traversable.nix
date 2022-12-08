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
      identifier = { name = "indexed-traversable"; version = "0.1.2"; };
      license = "BSD-2-Clause";
      copyright = "";
      maintainer = "Oleg Grenrus <oleg.grenrus@iki.fi>";
      author = "Edward Kmett";
      homepage = "";
      url = "";
      synopsis = "FunctorWithIndex, FoldableWithIndex, TraversableWithIndex";
      description = "This package provides three useful generalizations:\n\n@\nclass Functor f => FunctorWithIndex i f | f -> i where\n\\  imap :: (i -> a -> b) -> f a -> f b\n@\n\n@\nclass Foldable f => FoldableWithIndex i f | f -> i where\n\\  ifoldMap :: Monoid m => (i -> a -> m) -> f a -> m\n@\n\n@\nclass (FunctorWithIndex i t, FoldableWithIndex i t, Traversable t) => TraversableWithIndex i t | t -> i where\n\\  itraverse :: Applicative f => (i -> a -> f b) -> t a -> f (t b)\n@\n\nThis package contains instances for types in GHC boot libraries.\nFor some additional instances see [indexed-traversable-instances](https://hackage.haskell.org/package/indexed-traversable-instances).\n\nThe [keys](https://hackage.haskell.org/package/keys) package provides similar functionality,\nbut uses (associated) @TypeFamilies@ instead of @FunctionalDependencies@.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = (((([
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          ] ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "7.8")) (hsPkgs."tagged" or (errorHandler.buildDepError "tagged"))) ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "7.10")) (hsPkgs."void" or (errorHandler.buildDepError "void"))) ++ (pkgs.lib).optionals (!(compiler.isGhc && (compiler.version).ge "8.0")) [
          (hsPkgs."base-orphans" or (errorHandler.buildDepError "base-orphans"))
          (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
          (hsPkgs."transformers-compat" or (errorHandler.buildDepError "transformers-compat"))
          ]) ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).ge "7.0" && (compiler.isGhc && (compiler.version).lt "7.6")) (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))) ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).ge "7.0" && (compiler.isGhc && (compiler.version).lt "7.2")) (hsPkgs."generic-deriving" or (errorHandler.buildDepError "generic-deriving"));
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/indexed-traversable-0.1.2.tar.gz";
      sha256 = "516858ee7198b1fed1b93c665157f9855fd947379db7f115d48c1b0d670e698d";
      });
    }