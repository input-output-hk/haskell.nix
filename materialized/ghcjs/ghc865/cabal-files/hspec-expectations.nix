{ system
  , compiler
  , flags
  , pkgs
  , hsPkgs
  , pkgconfPkgs
  , errorHandler
  , config
  , ... }:
  ({
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = { name = "hspec-expectations"; version = "0.8.2"; };
      license = "MIT";
      copyright = "(c) 2011-2015 Simon Hengel";
      maintainer = "Simon Hengel <sol@typeful.net>";
      author = "Simon Hengel <sol@typeful.net>";
      homepage = "https://github.com/hspec/hspec-expectations#readme";
      url = "";
      synopsis = "Catchy combinators for HUnit";
      description = "Catchy combinators for HUnit: <https://github.com/hspec/hspec-expectations#readme>";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."call-stack" or (errorHandler.buildDepError "call-stack"))
          (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
          ];
        buildable = true;
        };
      tests = {
        "spec" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."call-stack" or (errorHandler.buildDepError "call-stack"))
            (hsPkgs."nanospec" or (errorHandler.buildDepError "nanospec"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/hspec-expectations-0.8.2.tar.gz";
      sha256 = "819607ea1faf35ce5be34be61c6f50f3389ea43892d56fb28c57a9f5d54fb4ef";
      });
    }) // {
    package-description-override = "-- This file has been generated from package.yaml by hpack version 0.15.0.\n--\n-- see: https://github.com/sol/hpack\n\nname:             hspec-expectations\nversion:          0.8.2\nsynopsis:         Catchy combinators for HUnit\ndescription:      Catchy combinators for HUnit: <https://github.com/hspec/hspec-expectations#readme>\nbug-reports:      https://github.com/hspec/hspec-expectations/issues\nlicense:          MIT\nlicense-file:     LICENSE\ncopyright:        (c) 2011-2015 Simon Hengel\nauthor:           Simon Hengel <sol@typeful.net>\nmaintainer:       Simon Hengel <sol@typeful.net>\nbuild-type:       Simple\ncategory:         Testing\ncabal-version:    >= 1.10\nhomepage:         https://github.com/hspec/hspec-expectations#readme\n\nsource-repository head\n  type: git\n  location: https://github.com/hspec/hspec-expectations\n\nlibrary\n  hs-source-dirs:\n      src\n  ghc-options: -Wall\n  build-depends:\n      base == 4.*\n    , call-stack\n    , HUnit\n  exposed-modules:\n      Test.Hspec.Expectations\n      Test.Hspec.Expectations.Contrib\n  other-modules:\n      Test.Hspec.Expectations.Matcher\n      Paths_hspec_expectations\n  default-language: Haskell2010\n\ntest-suite spec\n  type: exitcode-stdio-1.0\n  main-is: Spec.hs\n  hs-source-dirs:\n      test\n      src\n  ghc-options: -Wall\n  build-depends:\n      base == 4.*\n    , call-stack\n    , nanospec\n    , HUnit >= 1.5.0.0\n  other-modules:\n      Test.Hspec.Expectations.MatcherSpec\n      Test.Hspec.ExpectationsSpec\n      Test.Hspec.Expectations\n      Test.Hspec.Expectations.Contrib\n      Test.Hspec.Expectations.Matcher\n  default-language: Haskell2010\n";
    }