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
      identifier = { name = "hspec-core"; version = "2.7.8"; };
      license = "MIT";
      copyright = "(c) 2011-2021 Simon Hengel,\n(c) 2011-2012 Trystan Spangler,\n(c) 2011 Greg Weber";
      maintainer = "Simon Hengel <sol@typeful.net>";
      author = "";
      homepage = "http://hspec.github.io/";
      url = "";
      synopsis = "A Testing Framework for Haskell";
      description = "This package exposes internal types and functions that can be used to extend Hspec's functionality.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
          (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
          (hsPkgs."ansi-terminal" or (errorHandler.buildDepError "ansi-terminal"))
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."call-stack" or (errorHandler.buildDepError "call-stack"))
          (hsPkgs."clock" or (errorHandler.buildDepError "clock"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."hspec-expectations" or (errorHandler.buildDepError "hspec-expectations"))
          (hsPkgs."quickcheck-io" or (errorHandler.buildDepError "quickcheck-io"))
          (hsPkgs."random" or (errorHandler.buildDepError "random"))
          (hsPkgs."setenv" or (errorHandler.buildDepError "setenv"))
          (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
          (hsPkgs."tf-random" or (errorHandler.buildDepError "tf-random"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          ];
        buildable = true;
        };
      tests = {
        "spec" = {
          depends = [
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."ansi-terminal" or (errorHandler.buildDepError "ansi-terminal"))
            (hsPkgs."array" or (errorHandler.buildDepError "array"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."call-stack" or (errorHandler.buildDepError "call-stack"))
            (hsPkgs."clock" or (errorHandler.buildDepError "clock"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."hspec-expectations" or (errorHandler.buildDepError "hspec-expectations"))
            (hsPkgs."hspec-meta" or (errorHandler.buildDepError "hspec-meta"))
            (hsPkgs."process" or (errorHandler.buildDepError "process"))
            (hsPkgs."quickcheck-io" or (errorHandler.buildDepError "quickcheck-io"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."setenv" or (errorHandler.buildDepError "setenv"))
            (hsPkgs."silently" or (errorHandler.buildDepError "silently"))
            (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
            (hsPkgs."temporary" or (errorHandler.buildDepError "temporary"))
            (hsPkgs."tf-random" or (errorHandler.buildDepError "tf-random"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            ];
          build-tools = [
            (hsPkgs.pkgsBuildBuild.hspec-meta.components.exes.hspec-meta-discover or (pkgs.pkgsBuildBuild.hspec-meta-discover or (errorHandler.buildToolDepError "hspec-meta:hspec-meta-discover")))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/hspec-core-2.7.8.tar.gz";
      sha256 = "6daf1a671e97496a293e7020561ff8bb577b4e20cff6e2b21dfe6626f7568781";
      });
    }) // {
    package-description-override = "cabal-version: 1.12\r\n\r\n-- This file has been generated from package.yaml by hpack version 0.34.3.\r\n--\r\n-- see: https://github.com/sol/hpack\r\n\r\nname:             hspec-core\r\nversion:          2.7.8\r\nx-revision: 1\r\nlicense:          MIT\r\nlicense-file:     LICENSE\r\ncopyright:        (c) 2011-2021 Simon Hengel,\r\n                  (c) 2011-2012 Trystan Spangler,\r\n                  (c) 2011 Greg Weber\r\nmaintainer:       Simon Hengel <sol@typeful.net>\r\nbuild-type:       Simple\r\nextra-source-files:\r\n    version.yaml\r\ncategory:         Testing\r\nstability:        experimental\r\nbug-reports:      https://github.com/hspec/hspec/issues\r\nhomepage:         http://hspec.github.io/\r\nsynopsis:         A Testing Framework for Haskell\r\ndescription:      This package exposes internal types and functions that can be used to extend Hspec's functionality.\r\n\r\nsource-repository head\r\n  type: git\r\n  location: https://github.com/hspec/hspec\r\n  subdir: hspec-core\r\n\r\nlibrary\r\n  hs-source-dirs:\r\n      src\r\n      vendor\r\n  ghc-options: -Wall\r\n  build-depends:\r\n      HUnit ==1.6.*\r\n    , QuickCheck >=2.13.1\r\n    , ansi-terminal >=0.5\r\n    , array\r\n    , base >=4.5.0.0 && <5\r\n    , call-stack\r\n    , clock >=0.7.1\r\n    , deepseq\r\n    , directory\r\n    , filepath\r\n    , hspec-expectations ==0.8.2.*\r\n    , quickcheck-io >=0.2.0\r\n    , random\r\n    , setenv\r\n    , stm >=2.2\r\n    , tf-random\r\n    , transformers >=0.2.2.0\r\n  exposed-modules:\r\n      Test.Hspec.Core.Spec\r\n      Test.Hspec.Core.Hooks\r\n      Test.Hspec.Core.Runner\r\n      Test.Hspec.Core.Formatters\r\n      Test.Hspec.Core.QuickCheck\r\n      Test.Hspec.Core.Util\r\n  other-modules:\r\n      Test.Hspec.Core.Clock\r\n      Test.Hspec.Core.Compat\r\n      Test.Hspec.Core.Config\r\n      Test.Hspec.Core.Config.Options\r\n      Test.Hspec.Core.Config.Util\r\n      Test.Hspec.Core.Example\r\n      Test.Hspec.Core.Example.Location\r\n      Test.Hspec.Core.FailureReport\r\n      Test.Hspec.Core.Format\r\n      Test.Hspec.Core.Formatters.Diff\r\n      Test.Hspec.Core.Formatters.Free\r\n      Test.Hspec.Core.Formatters.Internal\r\n      Test.Hspec.Core.Formatters.Monad\r\n      Test.Hspec.Core.QuickCheckUtil\r\n      Test.Hspec.Core.Runner.Eval\r\n      Test.Hspec.Core.Shuffle\r\n      Test.Hspec.Core.Spec.Monad\r\n      Test.Hspec.Core.Timer\r\n      Test.Hspec.Core.Tree\r\n      Control.Concurrent.Async\r\n      Data.Algorithm.Diff\r\n      Paths_hspec_core\r\n  default-language: Haskell2010\r\n\r\ntest-suite spec\r\n  type: exitcode-stdio-1.0\r\n  main-is: Spec.hs\r\n  hs-source-dirs:\r\n      src\r\n      vendor\r\n      test\r\n  ghc-options: -Wall\r\n  cpp-options: -DTEST\r\n  build-depends:\r\n      HUnit ==1.6.*\r\n    , QuickCheck >=2.14\r\n    , ansi-terminal >=0.5\r\n    , array\r\n    , base >=4.5.0.0 && <5\r\n    , call-stack\r\n    , clock >=0.7.1\r\n    , deepseq\r\n    , directory\r\n    , filepath\r\n    , hspec-expectations ==0.8.2.*\r\n    , hspec-meta >=2.3.2 && < 2.7.8\r\n    , process\r\n    , quickcheck-io >=0.2.0\r\n    , random\r\n    , setenv\r\n    , silently >=1.2.4\r\n    , stm >=2.2\r\n    , temporary\r\n    , tf-random\r\n    , transformers >=0.2.2.0\r\n  build-tool-depends:\r\n      hspec-meta:hspec-meta-discover\r\n  other-modules:\r\n      Test.Hspec.Core.Clock\r\n      Test.Hspec.Core.Compat\r\n      Test.Hspec.Core.Config\r\n      Test.Hspec.Core.Config.Options\r\n      Test.Hspec.Core.Config.Util\r\n      Test.Hspec.Core.Example\r\n      Test.Hspec.Core.Example.Location\r\n      Test.Hspec.Core.FailureReport\r\n      Test.Hspec.Core.Format\r\n      Test.Hspec.Core.Formatters\r\n      Test.Hspec.Core.Formatters.Diff\r\n      Test.Hspec.Core.Formatters.Free\r\n      Test.Hspec.Core.Formatters.Internal\r\n      Test.Hspec.Core.Formatters.Monad\r\n      Test.Hspec.Core.Hooks\r\n      Test.Hspec.Core.QuickCheck\r\n      Test.Hspec.Core.QuickCheckUtil\r\n      Test.Hspec.Core.Runner\r\n      Test.Hspec.Core.Runner.Eval\r\n      Test.Hspec.Core.Shuffle\r\n      Test.Hspec.Core.Spec\r\n      Test.Hspec.Core.Spec.Monad\r\n      Test.Hspec.Core.Timer\r\n      Test.Hspec.Core.Tree\r\n      Test.Hspec.Core.Util\r\n      Control.Concurrent.Async\r\n      Data.Algorithm.Diff\r\n      All\r\n      Helper\r\n      Mock\r\n      Test.Hspec.Core.ClockSpec\r\n      Test.Hspec.Core.CompatSpec\r\n      Test.Hspec.Core.Config.OptionsSpec\r\n      Test.Hspec.Core.Config.UtilSpec\r\n      Test.Hspec.Core.ConfigSpec\r\n      Test.Hspec.Core.Example.LocationSpec\r\n      Test.Hspec.Core.ExampleSpec\r\n      Test.Hspec.Core.FailureReportSpec\r\n      Test.Hspec.Core.Formatters.DiffSpec\r\n      Test.Hspec.Core.FormattersSpec\r\n      Test.Hspec.Core.HooksSpec\r\n      Test.Hspec.Core.QuickCheckUtilSpec\r\n      Test.Hspec.Core.Runner.EvalSpec\r\n      Test.Hspec.Core.RunnerSpec\r\n      Test.Hspec.Core.ShuffleSpec\r\n      Test.Hspec.Core.SpecSpec\r\n      Test.Hspec.Core.TimerSpec\r\n      Test.Hspec.Core.UtilSpec\r\n      Paths_hspec_core\r\n  default-language: Haskell2010\r\n";
    }