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
      identifier = { name = "relude"; version = "1.0.0.1"; };
      license = "MIT";
      copyright = "2016 Stephen Diehl, 2016-2018 Serokell, 2018-2021 Kowainik";
      maintainer = "Kowainik <xrom.xkov@gmail.com>";
      author = "Dmitrii Kovanikov, Veronika Romashkina, Stephen Diehl, Serokell";
      homepage = "https://github.com/kowainik/relude";
      url = "";
      synopsis = "Safe, performant, user-friendly and lightweight Haskell Standard Library";
      description = "@__relude__@ is an alternative prelude library. If you find the default\n@Prelude@ unsatisfying, despite its advantages, consider using @relude@\ninstead.\n\n== Relude goals and design principles\n* __Productivity.__ You can be more productive with a \"non-standard\" standard\n  library, and @relude@ helps you with writing safer and more\n  efficient code faster.\n\n* __Total programming__. Usage of [/partial functions/](https://www.reddit.com/r/haskell/comments/5n51u3/why_are_partial_functions_as_in_head_tail_bad/)\n  can lead to unexpected bugs and runtime exceptions in pure\n  code. The types of partial functions lie about their behaviour. And\n  even if it is not always possible to rely only on total functions,\n  @relude@ strives to encourage best-practices and reduce the\n  chances of introducing a bug.\n\n    +---------------------------------+--------------------------------------------+\n    | __Partial__                     | __Total__                                  |\n    +=================================+============================================+\n    | @head :: [a] -> a@              | @head :: NonEmpty a -> a@                  |\n    +---------------------------------+--------------------------------------------+\n    | @tail :: [a] -> [a]@            | @tail :: NonEmpty a -> [a]@                |\n    +---------------------------------+--------------------------------------------+\n    | @read :: Read a => String -> a@ | @readMaybe :: Read a => String -> Maybe a@ |\n    +---------------------------------+--------------------------------------------+\n    | @fromJust :: Maybe a -> a@      | @fromMaybe :: a -> Maybe a -> a@           |\n    +---------------------------------+--------------------------------------------+\n\n* __Type-safety__. We use the /\"make invalid states unrepresentable\"/ motto as one\n  of our guiding principles. If it is possible, we express this concept through the\n  types.\n\n    /Example:/ @ whenNotNull :: Applicative f => [a] -> (NonEmpty a -> f ()) -> f () @\n\n* __Performance.__ We prefer @Text@ over @[String](https://www.reddit.com/r/haskell/comments/29jw0s/whats_wrong_with_string/)@,\n  use space-leaks-free functions (e.g. our custom performant @sum@ and @product@),\n  introduce @\\{\\-\\# INLINE \\#\\-\\}@ and @\\{\\-\\# SPECIALIZE \\#\\-\\}@ pragmas where\n  appropriate, and make efficient container types\n  (e.g. @Map@, @HashMap@, @Set@) more accesible.\n\n* __Minimalism__ (low number of dependencies). We do not force users of\n  @relude@ to stick to any specific lens or text formatting or logging\n  library. Where possible, @relude@ depends only on boot libraries.\n  The [Dependency graph](https://raw.githubusercontent.com/kowainik/relude/main/relude-dependency-graph.png)\n  of @relude@ can give you a clearer picture.\n\n* __Convenience__. Despite minimalism, we want to bring commonly used\n   types and functions into scope, and make available functions easier\n   to use. Some examples of conveniences:\n\n    1. No need to add @containers@, @unordered-containers@, @text@\n       and @bytestring@ to dependencies in your @.cabal@ file to\n       use the main API of these libraries\n    2. No need to import types like @NonEmpty@, @Text@, @Set@, @Reader[T]@, @MVar@, @STM@\n    3. Functions like @liftIO@, @fromMaybe@, @sortWith@ are avaiable by default as well\n    4. @IO@ actions are lifted to @MonadIO@\n\n* __Excellent documentation.__\n\n    1. Tutorial\n    2. Migration guide from @Prelude@\n    3. Haddock for every function with examples tested by\n       [doctest](http://hackage.haskell.org/package/doctest).\n    4. Documentation regarding [internal module structure](http://hackage.haskell.org/package/relude/docs/Relude.html)\n    5. @relude@-specific [HLint](http://hackage.haskell.org/package/hlint) rules: @[.hlint.yaml](https://github.com/kowainik/relude/blob/main/.hlint.yaml)@\n\n* __User-friendliness.__ Anyone should be able to quickly migrate to @relude@. Only\n  some basic familiarity with the common libraries like @text@ and @containers@\n  should be enough (but not necessary).\n\n* __Exploration.__ We have space to experiment with new ideas and proposals\n  without introducing breaking changes. @relude@ uses the approach with\n  @Extra.*@ modules which are not exported by default. The chosen approach makes it quite\n  easy for us to provide new functionality without breaking anything and let\n  the users decide to use it or not.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          ];
        buildable = true;
        };
      tests = {
        "relude-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."relude" or (errorHandler.buildDepError "relude"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            ];
          buildable = true;
          };
        "relude-doctest" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."relude" or (errorHandler.buildDepError "relude"))
            (hsPkgs."doctest" or (errorHandler.buildDepError "doctest"))
            (hsPkgs."Glob" or (errorHandler.buildDepError "Glob"))
            ];
          buildable = true;
          };
        };
      benchmarks = {
        "relude-benchmark" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."relude" or (errorHandler.buildDepError "relude"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/relude-1.0.0.1.tar.gz";
      sha256 = "4fdcfe29205ebe8a2b976339207cffeb5bede18f8d7b3332245ac5ed5e508933";
      });
    }