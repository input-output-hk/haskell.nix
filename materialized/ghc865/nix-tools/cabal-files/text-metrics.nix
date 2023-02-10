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
    flags = { dev = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "text-metrics"; version = "0.3.0"; };
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
      url = "http://hackage.haskell.org/package/text-metrics-0.3.0.tar.gz";
      sha256 = "3874af74060e35f01702640b353ac2180d93bb5d292a204e0ee3cadd26efbfa2";
      });
    }) // {
    package-description-override = "name:                 text-metrics\r\nversion:              0.3.0\r\nx-revision: 4\r\ncabal-version:        >= 1.10\r\ntested-with:          GHC==7.8.4, GHC==7.10.3, GHC==8.0.2, GHC==8.2.1\r\nlicense:              BSD3\r\nlicense-file:         LICENSE.md\r\nauthor:               Mark Karpov <markkarpov92@gmail.com>\r\nmaintainer:           Mark Karpov <markkarpov92@gmail.com>\r\nhomepage:             https://github.com/mrkkrp/text-metrics\r\nbug-reports:          https://github.com/mrkkrp/text-metrics/issues\r\ncategory:             Text, Algorithms\r\nsynopsis:             Calculate various string metrics efficiently\r\nbuild-type:           Simple\r\ndescription:          Calculate various string metrics efficiently.\r\nextra-doc-files:      CHANGELOG.md\r\n                    , README.md\r\n\r\nsource-repository head\r\n  type:               git\r\n  location:           https://github.com/mrkkrp/text-metrics.git\r\n\r\nflag dev\r\n  description:        Turn on development settings.\r\n  manual:             True\r\n  default:            False\r\n\r\nlibrary\r\n  build-depends:      base             >= 4.7 && < 5.0\r\n                    , containers       >= 0.5 && < 0.7\r\n                    , text             >= 0.2 && < 1.3\r\n                    , vector           >= 0.11 && < 0.13\r\n  exposed-modules:    Data.Text.Metrics\r\n  if flag(dev)\r\n    ghc-options:      -Wall -Werror\r\n  else\r\n    ghc-options:      -O2 -Wall\r\n  default-language:   Haskell2010\r\n\r\ntest-suite tests\r\n  main-is:            Main.hs\r\n  hs-source-dirs:     tests\r\n  type:               exitcode-stdio-1.0\r\n  build-depends:      QuickCheck       >= 2.8 && < 3.0\r\n                    , base             >= 4.7 && < 5.0\r\n                    , hspec            >= 2.0 && < 3.0\r\n                    , text             >= 0.2 && < 1.3\r\n                    , text-metrics\r\n  if flag(dev)\r\n    ghc-options:      -Wall -Werror\r\n  else\r\n    ghc-options:      -O2 -Wall\r\n  default-language:   Haskell2010\r\n\r\nbenchmark bench-speed\r\n  main-is:            Main.hs\r\n  hs-source-dirs:     bench-speed\r\n  type:               exitcode-stdio-1.0\r\n  build-depends:      base             >= 4.7 && < 5.0\r\n                    , criterion        >= 0.6.2.1 && < 1.6\r\n                    , deepseq          >= 1.4 && < 1.5\r\n                    , text             >= 0.2 && < 1.3\r\n                    , text-metrics\r\n  if flag(dev)\r\n    ghc-options:      -O2 -Wall -Werror\r\n  else\r\n    ghc-options:      -O2 -Wall\r\n  default-language:   Haskell2010\r\n\r\nbenchmark bench-memory\r\n  main-is:            Main.hs\r\n  hs-source-dirs:     bench-memory\r\n  type:               exitcode-stdio-1.0\r\n  build-depends:      base             >= 4.7 && < 5.0\r\n                    , deepseq          >= 1.4 && < 1.5\r\n                    , text             >= 0.2 && < 1.3\r\n                    , text-metrics\r\n                    , weigh            >= 0.0.4\r\n  if flag(dev)\r\n    ghc-options:      -O2 -Wall -Werror\r\n  else\r\n    ghc-options:      -O2 -Wall\r\n  default-language:   Haskell2010\r\n";
    }