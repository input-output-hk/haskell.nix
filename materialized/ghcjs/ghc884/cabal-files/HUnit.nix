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
      specVersion = "1.12";
      identifier = { name = "HUnit"; version = "1.6.1.0"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Simon Hengel <sol@typeful.net>";
      author = "Dean Herington";
      homepage = "https://github.com/hspec/HUnit#readme";
      url = "";
      synopsis = "A unit testing framework for Haskell";
      description = "HUnit is a unit testing framework for Haskell, inspired by the\nJUnit tool for Java, see: <http://www.junit.org>.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."call-stack" or (errorHandler.buildDepError "call-stack"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          ];
        buildable = true;
        };
      tests = {
        "tests" = {
          depends = [
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."call-stack" or (errorHandler.buildDepError "call-stack"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/HUnit-1.6.1.0.tar.gz";
      sha256 = "4384b38872dc1482db000e32af4808bb9a6fc0d1c39a9ad319db5f69b328f1e6";
      });
    }) // {
    package-description-override = "cabal-version: 1.12\r\n\r\n-- This file has been generated from package.yaml by hpack version 0.33.0.\r\n--\r\n-- see: https://github.com/sol/hpack\r\n\r\nname:                   HUnit\r\nversion:                1.6.1.0\r\nx-revision: 1\r\nlicense:                BSD3\r\nlicense-file:           LICENSE\r\nauthor:                 Dean Herington\r\nmaintainer:             Simon Hengel <sol@typeful.net>\r\nstability:              stable\r\nhomepage:               https://github.com/hspec/HUnit#readme\r\nbug-reports:            https://github.com/hspec/HUnit/issues\r\ncategory:               Testing\r\nsynopsis:               A unit testing framework for Haskell\r\ndescription:            HUnit is a unit testing framework for Haskell, inspired by the\r\n                        JUnit tool for Java, see: <http://www.junit.org>.\r\nbuild-type:             Simple\r\nextra-source-files:\r\n  CHANGELOG.md\r\n  README.md\r\n\r\nsource-repository head\r\n  type: git\r\n  location: https://github.com/hspec/HUnit\r\n\r\nlibrary\r\n  hs-source-dirs:\r\n    src\r\n  other-extensions: ConstraintKinds\r\n  build-depends:\r\n    base ==4.*,\r\n    call-stack,\r\n    deepseq\r\n  exposed-modules:\r\n    Test.HUnit.Base\r\n    Test.HUnit.Lang\r\n    Test.HUnit.Terminal\r\n    Test.HUnit.Text\r\n    Test.HUnit\r\n  other-modules:\r\n    Paths_HUnit\r\n  default-language: Haskell2010\r\n  ghc-options: -Wall\r\n\r\ntest-suite tests\r\n  type: exitcode-stdio-1.0\r\n  main-is: HUnitTests.hs\r\n  hs-source-dirs:\r\n    tests\r\n    examples\r\n  build-depends:\r\n    HUnit,\r\n    base ==4.*,\r\n    call-stack,\r\n    deepseq,\r\n    filepath\r\n  other-modules:\r\n    HUnitTestBase\r\n    HUnitTestExtended\r\n    TerminalTest\r\n    Example\r\n    Paths_HUnit\r\n  default-language: Haskell2010\r\n  ghc-options: -Wall\r\n";
    }