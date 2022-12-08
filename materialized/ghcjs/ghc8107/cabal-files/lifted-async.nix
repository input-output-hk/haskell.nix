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
      specVersion = "1.24";
      identifier = { name = "lifted-async"; version = "0.10.1.3"; };
      license = "BSD-3-Clause";
      copyright = "Copyright (C) 2012-2021 Mitsutoshi Aoe";
      maintainer = "Mitsutoshi Aoe <me@maoe.name>";
      author = "Mitsutoshi Aoe";
      homepage = "https://github.com/maoe/lifted-async";
      url = "";
      synopsis = "Run lifted IO operations asynchronously and wait for their results";
      description = "This package provides IO operations from @async@ package lifted to any\ninstance of 'MonadBase' or 'MonadBaseControl'.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."async" or (errorHandler.buildDepError "async"))
          (hsPkgs."lifted-base" or (errorHandler.buildDepError "lifted-base"))
          (hsPkgs."transformers-base" or (errorHandler.buildDepError "transformers-base"))
          (hsPkgs."monad-control" or (errorHandler.buildDepError "monad-control"))
          ] ++ [
          (hsPkgs."constraints" or (errorHandler.buildDepError "constraints"))
          ];
        buildable = true;
        };
      tests = {
        "test-lifted-async" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."lifted-async" or (errorHandler.buildDepError "lifted-async"))
            (hsPkgs."lifted-base" or (errorHandler.buildDepError "lifted-base"))
            (hsPkgs."monad-control" or (errorHandler.buildDepError "monad-control"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-expected-failure" or (errorHandler.buildDepError "tasty-expected-failure"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-th" or (errorHandler.buildDepError "tasty-th"))
            ];
          buildable = true;
          };
        "regression-tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."async" or (errorHandler.buildDepError "async"))
            (hsPkgs."lifted-async" or (errorHandler.buildDepError "lifted-async"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-th" or (errorHandler.buildDepError "tasty-th"))
            ];
          buildable = true;
          };
        };
      benchmarks = {
        "benchmark-lifted-async" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."async" or (errorHandler.buildDepError "async"))
            (hsPkgs."tasty-bench" or (errorHandler.buildDepError "tasty-bench"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."lifted-async" or (errorHandler.buildDepError "lifted-async"))
            ];
          buildable = true;
          };
        "benchmark-lifted-async-threaded" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."async" or (errorHandler.buildDepError "async"))
            (hsPkgs."tasty-bench" or (errorHandler.buildDepError "tasty-bench"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."lifted-async" or (errorHandler.buildDepError "lifted-async"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/lifted-async-0.10.1.3.tar.gz";
      sha256 = "f340fa9b649dd6bd3fc0942eceb94945a5b251e676b8d8e9841d6b24c531b4c2";
      });
    }