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
    flags = { bench = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "async"; version = "2.2.4"; };
      license = "BSD-3-Clause";
      copyright = "(c) Simon Marlow 2012";
      maintainer = "Simon Marlow <marlowsd@gmail.com>";
      author = "Simon Marlow";
      homepage = "https://github.com/simonmar/async";
      url = "";
      synopsis = "Run IO operations asynchronously and wait for their results";
      description = "This package provides a higher-level interface over\nthreads, in which an @Async a@ is a concurrent\nthread that will eventually deliver a value of\ntype @a@.  The package provides ways to create\n@Async@ computations, wait for their results, and\ncancel them.\n\nUsing @Async@ is safer than using threads in two\nways:\n\n* When waiting for a thread to return a result,\nif the thread dies with an exception then the\ncaller must either re-throw the exception\n('wait') or handle it ('waitCatch'); the\nexception cannot be ignored.\n\n* The API makes it possible to build a tree of\nthreads that are automatically killed when\ntheir parent dies (see 'withAsync').";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
          ];
        buildable = true;
        };
      exes = {
        "concasync" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."async" or (errorHandler.buildDepError "async"))
            (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
            ];
          buildable = if !flags.bench then false else true;
          };
        "conccancel" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."async" or (errorHandler.buildDepError "async"))
            (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
            ];
          buildable = if !flags.bench then false else true;
          };
        "race" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."async" or (errorHandler.buildDepError "async"))
            (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
            ];
          buildable = if !flags.bench then false else true;
          };
        };
      tests = {
        "test-async" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."async" or (errorHandler.buildDepError "async"))
            (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
            (hsPkgs."test-framework" or (errorHandler.buildDepError "test-framework"))
            (hsPkgs."test-framework-hunit" or (errorHandler.buildDepError "test-framework-hunit"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/async-2.2.4.tar.gz";
      sha256 = "484df85be0e76c4fed9376451e48e1d0c6e97952ce79735b72d54297e7e0a725";
      });
    }