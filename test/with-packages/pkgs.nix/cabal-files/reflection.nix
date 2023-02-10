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
    flags = { slow = false; template-haskell = true; };
    package = {
      specVersion = "1.10";
      identifier = { name = "reflection"; version = "2.1.6"; };
      license = "BSD-3-Clause";
      copyright = "2009-2013 Edward A. Kmett,\n2012 Elliott Hird,\n2004 Oleg Kiselyov and Chung-chieh Shan";
      maintainer = "Edward A. Kmett <ekmett@gmail.com>";
      author = "Edward A. Kmett, Elliott Hird, Oleg Kiselyov and Chung-chieh Shan";
      homepage = "http://github.com/ekmett/reflection";
      url = "";
      synopsis = "Reifies arbitrary terms into types that can be reflected back into terms";
      description = "This package addresses the /configuration problem/ which is\npropagating configurations that are available at run-time, allowing\nmultiple configurations to coexist without resorting to mutable\nglobal variables or 'System.IO.Unsafe.unsafePerformIO'.\n\nThat package is an implementation of the ideas presented in the\npaper \\\"Functional Pearl: Implicit Configurations\\\" by Oleg Kiselyov\nand Chung-chieh Shan (<http://okmij.org/ftp/Haskell/tr-15-04.pdf original paper>).\nHowever, the API has been streamlined to improve performance.\n\nAustin Seipp's tutorial <https://www.schoolofhaskell.com/user/thoughtpolice/using-reflection Reflecting values to types and back> provides a summary of the\napproach taken by this library, along with more motivating examples.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = (([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          ] ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).lt "7.8") (hsPkgs."tagged" or (errorHandler.buildDepError "tagged"))) ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8.0")) (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))) ++ (pkgs.lib).optional (flags.template-haskell && (compiler.isGhc && true)) (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"));
        buildable = true;
        };
      tests = {
        "spec" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."reflection" or (errorHandler.buildDepError "reflection"))
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
      url = "http://hackage.haskell.org/package/reflection-2.1.6.tar.gz";
      sha256 = "bf3e14917ebb329a53701a3cce0afe670f20037a0148dbfa5cbfa574ed6ba6cd";
      });
    }) // {
    package-description-override = "name:           reflection\r\nversion:        2.1.6\r\nx-revision: 1\r\nlicense:        BSD3\r\nlicense-file:   LICENSE\r\nauthor:         Edward A. Kmett, Elliott Hird, Oleg Kiselyov and Chung-chieh Shan\r\nmaintainer:     Edward A. Kmett <ekmett@gmail.com>\r\nstability:      experimental\r\nhomepage:       http://github.com/ekmett/reflection\r\nbug-reports:    http://github.com/ekmett/reflection/issues\r\ncategory:       Data, Reflection, Dependent Types\r\nsynopsis:       Reifies arbitrary terms into types that can be reflected back into terms\r\ncopyright:      2009-2013 Edward A. Kmett,\r\n                2012 Elliott Hird,\r\n                2004 Oleg Kiselyov and Chung-chieh Shan\r\nbuild-type:     Simple\r\ncabal-version:  >= 1.10\r\ndescription:\r\n  This package addresses the /configuration problem/ which is\r\n  propagating configurations that are available at run-time, allowing\r\n  multiple configurations to coexist without resorting to mutable\r\n  global variables or 'System.IO.Unsafe.unsafePerformIO'.\r\n  .\r\n  That package is an implementation of the ideas presented in the\r\n  paper \\\"Functional Pearl: Implicit Configurations\\\" by Oleg Kiselyov\r\n  and Chung-chieh Shan (<http://okmij.org/ftp/Haskell/tr-15-04.pdf original paper>).\r\n  However, the API has been streamlined to improve performance.\r\n  .\r\n  Austin Seipp's tutorial <https://www.schoolofhaskell.com/user/thoughtpolice/using-reflection Reflecting values to types and back> provides a summary of the\r\n  approach taken by this library, along with more motivating examples.\r\ntested-with:   GHC == 7.0.4\r\n             , GHC == 7.2.2\r\n             , GHC == 7.4.2\r\n             , GHC == 7.6.3\r\n             , GHC == 7.8.4\r\n             , GHC == 7.10.3\r\n             , GHC == 8.0.2\r\n             , GHC == 8.2.2\r\n             , GHC == 8.4.4\r\n             , GHC == 8.6.5\r\n             , GHC == 8.8.3\r\n             , GHC == 8.10.1\r\n\r\nextra-source-files:\r\n  examples/reflection-examples.cabal\r\n  examples/LICENSE\r\n  examples/*.hs\r\n  CHANGELOG.markdown\r\n  README.markdown\r\n  slow/Data/Reflection.hs\r\n  fast/Data/Reflection.hs\r\n  .travis.yml\r\n\r\nflag slow\r\n  description:\r\n    If you enable this flag, we use a more portable much much slower implementation. Moreover, the 'Given' API is broken, so this is currently an unsupported configuration. If you feel the need to turn on this flag for any reason, please email the maintainer!\r\n  default: False\r\n  manual: False\r\n\r\nflag template-haskell\r\n  description:\r\n    You can disable the use of the `template-haskell` package using `-f-template-haskell`.\r\n    .\r\n    Disabling this is an unsupported configuration, but it may be useful for accelerating builds in sandboxes for expert users.\r\n  default: True\r\n  manual: True\r\n\r\nsource-repository head\r\n  type: git\r\n  location: git://github.com/ekmett/reflection.git\r\n\r\nlibrary\r\n  ghc-options: -Wall\r\n\r\n  if impl(ghc >= 7.2)\r\n    default-extensions: Trustworthy\r\n\r\n  build-depends:\r\n    base >= 2 && < 5\r\n\r\n  if impl(ghc < 7.8)\r\n    build-depends:\r\n      tagged >= 0.4.4 && < 1\r\n\r\n  if !impl(ghc >= 8.0)\r\n    build-depends:\r\n      semigroups >= 0.11 && < 0.21\r\n\r\n  default-language: Haskell98\r\n\r\n  if flag(template-haskell) && impl(ghc)\r\n    if !impl(ghc >= 8.0)\r\n      other-extensions: TemplateHaskell\r\n    -- else\r\n    --   other-extensions: TemplateHaskellQuotes -- Hackage doesn't know this extension yet\r\n    build-depends: template-haskell\r\n\r\n  if !flag(slow) && (impl(ghc) || impl(hugs))\r\n    hs-source-dirs: fast\r\n  else\r\n    other-extensions: ScopedTypeVariables, FlexibleInstances\r\n    hs-source-dirs: slow\r\n\r\n  other-extensions:\r\n    MultiParamTypeClasses,\r\n    FunctionalDependencies,\r\n    Rank2Types,\r\n    CPP\r\n\r\n  exposed-modules: Data.Reflection\r\n\r\ntest-suite spec\r\n  type: exitcode-stdio-1.0\r\n  hs-source-dirs: tests\r\n  main-is: Spec.hs\r\n  other-modules: ReifyNatSpec\r\n                 T47Spec\r\n  ghc-options: -Wall\r\n  default-language: Haskell98\r\n  build-tool-depends: hspec-discover:hspec-discover >= 1.8\r\n  build-depends:\r\n    base       >= 2   && < 5,\r\n    containers >= 0.1 && < 0.7,\r\n    hspec      >= 2   && < 3,\r\n    QuickCheck >= 2   && < 3,\r\n    reflection\r\n";
    }