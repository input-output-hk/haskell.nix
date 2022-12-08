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
    flags = { optimised-mixer = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "splitmix"; version = "0.1.0.4"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Oleg Grenrus <oleg.grenrus@iki.fi>";
      author = "";
      homepage = "";
      url = "";
      synopsis = "Fast Splittable PRNG";
      description = "Pure Haskell implementation of SplitMix described in\n\nGuy L. Steele, Jr., Doug Lea, and Christine H. Flood. 2014.\nFast splittable pseudorandom number generators. In Proceedings\nof the 2014 ACM International Conference on Object Oriented\nProgramming Systems Languages & Applications (OOPSLA '14). ACM,\nNew York, NY, USA, 453-472. DOI:\n<https://doi.org/10.1145/2660193.2660195>\n\nThe paper describes a new algorithm /SplitMix/ for /splittable/\npseudorandom number generator that is quite fast: 9 64 bit arithmetic/logical\noperations per 64 bits generated.\n\n/SplitMix/ is tested with two standard statistical test suites (DieHarder and\nTestU01, this implementation only using the former) and it appears to be\nadequate for \"everyday\" use, such as Monte Carlo algorithms and randomized\ndata structures where speed is important.\n\nIn particular, it __should not be used for cryptographic or security applications__,\nbecause generated sequences of pseudorandom values are too predictable\n(the mixing functions are easily inverted, and two successive outputs\nsuffice to reconstruct the internal state).";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          ] ++ (pkgs.lib).optionals (!(compiler.isGhcjs && true)) ((pkgs.lib).optional (!(compiler.isGhc && true)) (hsPkgs."time" or (errorHandler.buildDepError "time")));
        buildable = true;
        };
      tests = {
        "examples" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."splitmix" or (errorHandler.buildDepError "splitmix"))
            ];
          buildable = true;
          };
        "splitmix-tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base-compat" or (errorHandler.buildDepError "base-compat"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."math-functions" or (errorHandler.buildDepError "math-functions"))
            (hsPkgs."splitmix" or (errorHandler.buildDepError "splitmix"))
            (hsPkgs."test-framework" or (errorHandler.buildDepError "test-framework"))
            (hsPkgs."test-framework-hunit" or (errorHandler.buildDepError "test-framework-hunit"))
            ];
          buildable = true;
          };
        "montecarlo-pi" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."splitmix" or (errorHandler.buildDepError "splitmix"))
            ];
          buildable = true;
          };
        "montecarlo-pi-32" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."splitmix" or (errorHandler.buildDepError "splitmix"))
            ];
          buildable = true;
          };
        "splitmix-dieharder" = {
          depends = [
            (hsPkgs."async" or (errorHandler.buildDepError "async"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base-compat-batteries" or (errorHandler.buildDepError "base-compat-batteries"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."process" or (errorHandler.buildDepError "process"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."splitmix" or (errorHandler.buildDepError "splitmix"))
            (hsPkgs."tf-random" or (errorHandler.buildDepError "tf-random"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            ];
          buildable = true;
          };
        "splitmix-testu01" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base-compat-batteries" or (errorHandler.buildDepError "base-compat-batteries"))
            (hsPkgs."splitmix" or (errorHandler.buildDepError "splitmix"))
            ];
          libs = [ (pkgs."testu01" or (errorHandler.sysDepError "testu01")) ];
          buildable = if !system.isLinux then false else true;
          };
        "initialization" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."splitmix" or (errorHandler.buildDepError "splitmix"))
            ];
          buildable = true;
          };
        };
      benchmarks = {
        "comparison" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."splitmix" or (errorHandler.buildDepError "splitmix"))
            (hsPkgs."tf-random" or (errorHandler.buildDepError "tf-random"))
            ];
          buildable = true;
          };
        "simple-sum" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."splitmix" or (errorHandler.buildDepError "splitmix"))
            ];
          buildable = true;
          };
        "range" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."clock" or (errorHandler.buildDepError "clock"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."splitmix" or (errorHandler.buildDepError "splitmix"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/splitmix-0.1.0.4.tar.gz";
      sha256 = "6d065402394e7a9117093dbb4530a21342c9b1e2ec509516c8a8d0ffed98ecaa";
      });
    }