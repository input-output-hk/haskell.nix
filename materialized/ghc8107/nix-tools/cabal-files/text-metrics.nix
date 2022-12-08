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
    flags = { dev = false; };
    package = {
      specVersion = "2.4";
      identifier = { name = "text-metrics"; version = "0.3.2"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Mark Karpov <markkarpov92@gmail.com>";
      author = "Mark Karpov <markkarpov92@gmail.com>";
      homepage = "https://github.com/mrkkrp/text-metrics";
      url = "";
      synopsis = "Calculate various string metrics efficiently";
      description = "Calculate various string metrics efficiently.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          ];
        buildable = true;
        };
      tests = {
        "tests" = {
          depends = [
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."text-metrics" or (errorHandler.buildDepError "text-metrics"))
            ];
          buildable = true;
          };
        };
      benchmarks = {
        "bench-speed" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."text-metrics" or (errorHandler.buildDepError "text-metrics"))
            ];
          buildable = true;
          };
        "bench-memory" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."text-metrics" or (errorHandler.buildDepError "text-metrics"))
            (hsPkgs."weigh" or (errorHandler.buildDepError "weigh"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/text-metrics-0.3.2.tar.gz";
      sha256 = "bdd730a8f1ebc98a2d56edd73d94ebba6062ee1b49d4d019ffc2c37eaadd836e";
      });
    }