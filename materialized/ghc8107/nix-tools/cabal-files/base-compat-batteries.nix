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
      identifier = { name = "base-compat-batteries"; version = "0.12.2"; };
      license = "MIT";
      copyright = "(c) 2012-2018 Simon Hengel,\n(c) 2014-2018 João Cristóvão,\n(c) 2015-2018 Ryan Scott";
      maintainer = "Simon Hengel <sol@typeful.net>,\nJoão Cristóvão <jmacristovao@gmail.com>,\nRyan Scott <ryan.gl.scott@gmail.com>";
      author = "Simon Hengel <sol@typeful.net>,\nJoão Cristóvão <jmacristovao@gmail.com>,\nRyan Scott <ryan.gl.scott@gmail.com>";
      homepage = "";
      url = "";
      synopsis = "base-compat with extra batteries";
      description = "Provides functions available in later versions of @base@ to\na wider range of compilers, without requiring you to use CPP\npragmas in your code.\n\nThis package provides the same API as the\n@<http://hackage.haskell.org/package/base-compat base-compat>@\nlibrary, but depends on compatibility packages\n(such as @semigroups@) to offer a wider support window than\n@base-compat@, which has no dependencies. Most of the modules\nin this library have the same names as in @base-compat@\nto make it easier to switch between the two. There also exist\nversions of each module with the suffix @.Repl.Batteries@,\nwhich are distinct from anything in @base-compat@, to allow\nfor easier use in GHCi.\n\nSee\n@<https://github.com/haskell-compat/base-compat/blob/master/base-compat/README.markdown#dependencies here>@\nfor a more comprehensive list of differences between\n@base-compat@ and @base-compat-batteries@.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = ((((([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."base-compat" or (errorHandler.buildDepError "base-compat"))
          (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
          ] ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "7.8")) (hsPkgs."tagged" or (errorHandler.buildDepError "tagged"))) ++ (pkgs.lib).optionals (!(compiler.isGhc && (compiler.version).ge "7.10")) [
          (hsPkgs."nats" or (errorHandler.buildDepError "nats"))
          (hsPkgs."void" or (errorHandler.buildDepError "void"))
          ]) ++ (pkgs.lib).optionals (!(compiler.isGhc && (compiler.version).ge "8.0")) [
          (hsPkgs."fail" or (errorHandler.buildDepError "fail"))
          (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."transformers-compat" or (errorHandler.buildDepError "transformers-compat"))
          ]) ++ (pkgs.lib).optionals (!(compiler.isGhc && (compiler.version).ge "8.2")) [
          (hsPkgs."bifunctors" or (errorHandler.buildDepError "bifunctors"))
          (hsPkgs."type-equality" or (errorHandler.buildDepError "type-equality"))
          ]) ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8.6")) (hsPkgs."contravariant" or (errorHandler.buildDepError "contravariant"))) ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "9.0")) (hsPkgs."OneTuple" or (errorHandler.buildDepError "OneTuple"));
        buildable = true;
        };
      tests = {
        "spec" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base-compat-batteries" or (errorHandler.buildDepError "base-compat-batteries"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            ];
          build-tools = [
            (hsPkgs.buildPackages.hspec-discover.components.exes.hspec-discover or (pkgs.buildPackages.hspec-discover or (errorHandler.buildToolDepError "hspec-discover:hspec-discover")))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/base-compat-batteries-0.12.2.tar.gz";
      sha256 = "ede9092e07f904e0759160bf1ecd3fb7eb043bae6dc89a37c3dc94829ec5eb99";
      });
    }