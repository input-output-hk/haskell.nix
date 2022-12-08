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
      identifier = { name = "random"; version = "1.2.0"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "core-libraries-committee@haskell.org";
      author = "";
      homepage = "";
      url = "";
      synopsis = "Pseudo-random number generation";
      description = "This package provides basic pseudo-random number generation, including the\nability to split random number generators.\n\n== \"System.Random\": pure pseudo-random number interface\n\nIn pure code, use 'System.Random.uniform' and 'System.Random.uniformR' from\n\"System.Random\" to generate pseudo-random numbers with a pure pseudo-random\nnumber generator like 'System.Random.StdGen'.\n\nAs an example, here is how you can simulate rolls of a six-sided die using\n'System.Random.uniformR':\n\n>>> let roll = uniformR (1, 6)        :: RandomGen g => g -> (Word, g)\n>>> let rolls = unfoldr (Just . roll) :: RandomGen g => g -> [Word]\n>>> let pureGen = mkStdGen 42\n>>> take 10 (rolls pureGen)           :: [Word]\n[1,1,3,2,4,5,3,4,6,2]\n\nSee \"System.Random\" for more details.\n\n== \"System.Random.Stateful\": monadic pseudo-random number interface\n\nIn monadic code, use 'System.Random.Stateful.uniformM' and\n'System.Random.Stateful.uniformRM' from \"System.Random.Stateful\" to generate\npseudo-random numbers with a monadic pseudo-random number generator, or\nusing a monadic adapter.\n\nAs an example, here is how you can simulate rolls of a six-sided die using\n'System.Random.Stateful.uniformRM':\n\n>>> let rollM = uniformRM (1, 6)                 :: StatefulGen g m => g -> m Word\n>>> let pureGen = mkStdGen 42\n>>> runStateGen_ pureGen (replicateM 10 . rollM) :: [Word]\n[1,1,3,2,4,5,3,4,6,2]\n\nThe monadic adapter 'System.Random.Stateful.runGenState_' is used here to lift\nthe pure pseudo-random number generator @pureGen@ into the\n'System.Random.Stateful.StatefulGen' context.\n\nThe monadic interface can also be used with existing monadic pseudo-random\nnumber generators. In this example, we use the one provided in the\n<https://hackage.haskell.org/package/mwc-random mwc-random> package:\n\n>>> import System.Random.MWC as MWC\n>>> let rollM = uniformRM (1, 6)       :: StatefulGen g m => g -> m Word\n>>> monadicGen <- MWC.create\n>>> replicateM 10 (rollM monadicGen) :: IO [Word]\n[2,3,6,6,4,4,3,1,5,4]\n\nSee \"System.Random.Stateful\" for more details.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."splitmix" or (errorHandler.buildDepError "splitmix"))
          ] ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).lt "8.0") (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"));
        buildable = true;
        };
      tests = {
        "legacy-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            ];
          buildable = true;
          };
        "doctests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."doctest" or (errorHandler.buildDepError "doctest"))
            (hsPkgs."mwc-random" or (errorHandler.buildDepError "mwc-random"))
            (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."unliftio" or (errorHandler.buildDepError "unliftio"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            ];
          buildable = true;
          };
        "spec" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."smallcheck" or (errorHandler.buildDepError "smallcheck"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-smallcheck" or (errorHandler.buildDepError "tasty-smallcheck"))
            (hsPkgs."tasty-expected-failure" or (errorHandler.buildDepError "tasty-expected-failure"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            ];
          buildable = true;
          };
        };
      benchmarks = {
        "legacy-bench" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."rdtsc" or (errorHandler.buildDepError "rdtsc"))
            (hsPkgs."split" or (errorHandler.buildDepError "split"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            ];
          buildable = true;
          };
        "bench" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."gauge" or (errorHandler.buildDepError "gauge"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."splitmix" or (errorHandler.buildDepError "splitmix"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/random-1.2.0.tar.gz";
      sha256 = "e4519cf7c058bfd5bdbe4acc782284acc9e25e74487208619ca83cbcd63fb9de";
      });
    }