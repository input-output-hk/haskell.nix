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
      identifier = { name = "quickcheck-io"; version = "0.2.0"; };
      license = "MIT";
      copyright = "(c) 2013-2017 Simon Hengel";
      maintainer = "Simon Hengel <sol@typeful.net>";
      author = "Simon Hengel <sol@typeful.net>";
      homepage = "https://github.com/hspec/quickcheck-io#readme";
      url = "";
      synopsis = "Use HUnit assertions as QuickCheck properties";
      description = "This package provides an orphan instance that allows you to\nuse HUnit assertions as QuickCheck properties.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
          (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/quickcheck-io-0.2.0.tar.gz";
      sha256 = "fb779119d79fe08ff4d502fb6869a70c9a8d5fd8ae0959f605c3c937efd96422";
      });
    }) // {
    package-description-override = "-- This file has been generated from package.yaml by hpack version 0.18.0.\n--\n-- see: https://github.com/sol/hpack\n\nname:             quickcheck-io\nversion:          0.2.0\nsynopsis:         Use HUnit assertions as QuickCheck properties\ndescription:      This package provides an orphan instance that allows you to\n                  use HUnit assertions as QuickCheck properties.\ncategory:         Testing\nhomepage:         https://github.com/hspec/quickcheck-io#readme\nbug-reports:      https://github.com/hspec/quickcheck-io/issues\nlicense:          MIT\nlicense-file:     LICENSE\ncopyright:        (c) 2013-2017 Simon Hengel\nauthor:           Simon Hengel <sol@typeful.net>\nmaintainer:       Simon Hengel <sol@typeful.net>\nbuild-type:       Simple\ncabal-version:    >= 1.10\n\nsource-repository head\n  type: git\n  location: https://github.com/hspec/quickcheck-io\n\nlibrary\n  hs-source-dirs:\n      src\n  ghc-options: -Wall\n  build-depends:\n      QuickCheck  >= 2.7\n    , HUnit       >= 1.2.5\n    , base        == 4.*\n  exposed-modules:\n      Test.QuickCheck.IO\n  other-modules:\n      Paths_quickcheck_io\n  default-language: Haskell2010\n";
    }