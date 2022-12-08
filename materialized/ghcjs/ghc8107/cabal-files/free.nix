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
      specVersion = "1.18";
      identifier = { name = "free"; version = "5.1.3"; };
      license = "BSD-3-Clause";
      copyright = "Copyright (C) 2008-2015 Edward A. Kmett";
      maintainer = "Edward A. Kmett <ekmett@gmail.com>";
      author = "Edward A. Kmett";
      homepage = "http://github.com/ekmett/free/";
      url = "";
      synopsis = "Monads for free";
      description = "Free monads are useful for many tree-like structures and domain specific languages.\n\nIf @f@ is a 'Functor' then the free 'Monad' on @f@ is the type\nof trees whose nodes are labeled with the constructors of @f@. The word\n\\\"free\\\" is used in the sense of \\\"unrestricted\\\" rather than \\\"zero-cost\\\":\n@Free f@ makes no constraining assumptions beyond those given by @f@ and the\ndefinition of 'Monad'. As used here it is a standard term from the\nmathematical theory of adjoint functors.\n\nCofree comonads are dual to free monads. They provide convenient ways to talk\nabout branching streams and rose-trees, and can be used to annotate syntax\ntrees. The cofree comonad can be seen as a stream parameterized by a 'Functor'\nthat controls its branching factor.\n\nMore information on free monads, including examples, can be found in the\nfollowing blog posts:\n<http://comonad.com/reader/2008/monads-for-free/>\n<http://comonad.com/reader/2011/free-monads-for-less/>";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = ((([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."comonad" or (errorHandler.buildDepError "comonad"))
          (hsPkgs."distributive" or (errorHandler.buildDepError "distributive"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."profunctors" or (errorHandler.buildDepError "profunctors"))
          (hsPkgs."semigroupoids" or (errorHandler.buildDepError "semigroupoids"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."transformers-base" or (errorHandler.buildDepError "transformers-base"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          ] ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8.2")) (hsPkgs."bifunctors" or (errorHandler.buildDepError "bifunctors"))) ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8.0")) (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))) ++ (if compiler.isGhc && (compiler.version).ge "7.10"
          then [
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            ]
          else [
            (hsPkgs."transformers-compat" or (errorHandler.buildDepError "transformers-compat"))
            ])) ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8.0")) (hsPkgs."fail" or (errorHandler.buildDepError "fail"));
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/free-5.1.3.tar.gz";
      sha256 = "2c70d66e3a1ad52ce4b22d5510ffc6d7b3db950bd7f43bc61801cfe7b24c2e2d";
      });
    }