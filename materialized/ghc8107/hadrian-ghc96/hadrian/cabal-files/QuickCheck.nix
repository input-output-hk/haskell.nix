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
    flags = { templatehaskell = true; old-random = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "QuickCheck"; version = "2.14.3"; };
      license = "BSD-3-Clause";
      copyright = "2000-2019 Koen Claessen, 2006-2008 Björn Bringert, 2009-2019 Nick Smallbone";
      maintainer = "Nick Smallbone <nick@smallbone.se>";
      author = "Koen Claessen <koen@chalmers.se>";
      homepage = "https://github.com/nick8325/quickcheck";
      url = "";
      synopsis = "Automatic testing of Haskell programs";
      description = "QuickCheck is a library for random testing of program properties.\nThe programmer provides a specification of the program, in the form of\nproperties which functions should satisfy, and QuickCheck then tests that the\nproperties hold in a large number of randomly generated cases.\nSpecifications are expressed in Haskell, using combinators provided by\nQuickCheck. QuickCheck provides combinators to define properties, observe the\ndistribution of test data, and define test data generators.\n\nMost of QuickCheck's functionality is exported by the main \"Test.QuickCheck\"\nmodule. The main exception is the monadic property testing library in\n\"Test.QuickCheck.Monadic\".\n\nIf you are new to QuickCheck, you can try looking at the following resources:\n\n* The <http://www.cse.chalmers.se/~rjmh/QuickCheck/manual.html official QuickCheck manual>.\nIt's a bit out-of-date in some details and doesn't cover newer QuickCheck features,\nbut is still full of good advice.\n* <https://begriffs.com/posts/2017-01-14-design-use-quickcheck.html>,\na detailed tutorial written by a user of QuickCheck.\n\nThe <https://hackage.haskell.org/package/quickcheck-instances quickcheck-instances>\ncompanion package provides instances for types in Haskell Platform packages\nat the cost of additional dependencies.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = ((((((([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          ] ++ [
          (hsPkgs."random" or (errorHandler.buildDepError "random"))
          ]) ++ (pkgs.lib).optional (!(compiler.isHugs && true)) (hsPkgs."splitmix" or (errorHandler.buildDepError "splitmix"))) ++ (pkgs.lib).optionals (compiler.isGhc && true) [
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          ]) ++ (pkgs.lib).optional (compiler.isGhc && true && flags.templatehaskell) (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))) ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).ge "7.2" && (compiler.isGhc && (compiler.version).lt "7.6")) (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))) ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).ge "7.2") (hsPkgs."random" or (errorHandler.buildDepError "random"))) ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).ge "7.4") (hsPkgs."containers" or (errorHandler.buildDepError "containers"))) ++ (pkgs.lib).optionals (compiler.isUhc && true) [
          (hsPkgs."old-time" or (errorHandler.buildDepError "old-time"))
          (hsPkgs."old-locale" or (errorHandler.buildDepError "old-locale"))
          ];
        buildable = true;
        };
      tests = {
        "test-quickcheck" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            ];
          buildable = if !flags.templatehaskell then false else true;
          };
        "test-quickcheck-gcoarbitrary" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            ] ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).ge "7.2" && (compiler.isGhc && (compiler.version).lt "7.6")) (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"));
          buildable = if !flags.templatehaskell || !(compiler.isGhc && (compiler.version).ge "7.2")
            then false
            else true;
          };
        "test-quickcheck-generators" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            ];
          buildable = if !flags.templatehaskell then false else true;
          };
        "test-quickcheck-gshrink" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            ] ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).ge "7.2" && (compiler.isGhc && (compiler.version).lt "7.6")) (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"));
          buildable = if !flags.templatehaskell || !(compiler.isGhc && (compiler.version).ge "7.2")
            then false
            else true;
          };
        "test-quickcheck-terminal" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."process" or (errorHandler.buildDepError "process"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            ];
          buildable = if !flags.templatehaskell || !(compiler.isGhc && (compiler.version).ge "7.10")
            then false
            else true;
          };
        "test-quickcheck-monadfix" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            ];
          buildable = if !flags.templatehaskell || !(compiler.isGhc && (compiler.version).ge "7.10")
            then false
            else true;
          };
        "test-quickcheck-split" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            ];
          buildable = true;
          };
        "test-quickcheck-misc" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            ];
          buildable = if !flags.templatehaskell || !(compiler.isGhc && (compiler.version).ge "7.10")
            then false
            else true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/QuickCheck-2.14.3.tar.gz";
      sha256 = "5c0f22b36b28a1a8fa110b3819818d3f29494a3b0dedbae299f064123ca70501";
      });
    }) // {
    package-description-override = "Name: QuickCheck\nVersion: 2.14.3\nCabal-Version: >= 1.10\nBuild-type: Simple\nLicense: BSD3\nLicense-file: LICENSE\nCopyright: 2000-2019 Koen Claessen, 2006-2008 Björn Bringert, 2009-2019 Nick Smallbone\nAuthor: Koen Claessen <koen@chalmers.se>\nMaintainer: Nick Smallbone <nick@smallbone.se>\nBug-reports: https://github.com/nick8325/quickcheck/issues\nTested-with: GHC ==7.0.4 || ==7.2.2 || >= 7.4\nHomepage: https://github.com/nick8325/quickcheck\nCategory:       Testing\nSynopsis:       Automatic testing of Haskell programs\nDescription:\n  QuickCheck is a library for random testing of program properties.\n  The programmer provides a specification of the program, in the form of\n  properties which functions should satisfy, and QuickCheck then tests that the\n  properties hold in a large number of randomly generated cases.\n  Specifications are expressed in Haskell, using combinators provided by\n  QuickCheck. QuickCheck provides combinators to define properties, observe the\n  distribution of test data, and define test data generators.\n  .\n  Most of QuickCheck's functionality is exported by the main \"Test.QuickCheck\"\n  module. The main exception is the monadic property testing library in\n  \"Test.QuickCheck.Monadic\".\n  .\n  If you are new to QuickCheck, you can try looking at the following resources:\n  .\n  * The <http://www.cse.chalmers.se/~rjmh/QuickCheck/manual.html official QuickCheck manual>.\n    It's a bit out-of-date in some details and doesn't cover newer QuickCheck features,\n    but is still full of good advice.\n  * <https://begriffs.com/posts/2017-01-14-design-use-quickcheck.html>,\n    a detailed tutorial written by a user of QuickCheck.\n  .\n  The <https://hackage.haskell.org/package/quickcheck-instances quickcheck-instances>\n  companion package provides instances for types in Haskell Platform packages\n  at the cost of additional dependencies.\n\nextra-source-files:\n  README\n  changelog\n  examples/Heap.hs\n  examples/Heap_Program.hs\n  examples/Heap_ProgramAlgebraic.hs\n  examples/Lambda.hs\n  examples/Merge.hs\n  examples/Set.hs\n  examples/Simple.hs\n  make-hugs\n  test-hugs\n\nsource-repository head\n  type:     git\n  location: https://github.com/nick8325/quickcheck\n\nsource-repository this\n  type:     git\n  location: https://github.com/nick8325/quickcheck\n  tag:      2.14.3\n\nflag templateHaskell\n  Description: Build Test.QuickCheck.All, which uses Template Haskell.\n  Default: True\n  Manual: True\n\nflag old-random\n  Description: Build against a pre-1.2.0 version of the random package.\n  Default: False\n  Manual: False\n\nlibrary\n  Hs-source-dirs: src\n  Build-depends: base >=4.3 && <5, containers\n  Default-language: Haskell2010\n\n  -- New vs old random.\n  if flag(old-random)\n    Build-depends: random >= 1.0.0.3 && < 1.2.0\n    cpp-options: -DOLD_RANDOM\n  else\n    Build-depends: random >= 1.2.0 && < 1.3\n\n  -- We always use splitmix directly rather than going through StdGen\n  -- (it's somewhat more efficient).\n  -- However, Hugs traps overflow on Word64, so we have to stick\n  -- with StdGen there.\n  if impl(hugs)\n    cpp-options: -DNO_SPLITMIX\n  else\n    Build-depends: splitmix >= 0.1 && <0.2\n\n  -- Modules that are always built.\n  Exposed-Modules:\n    Test.QuickCheck,\n    Test.QuickCheck.Arbitrary,\n    Test.QuickCheck.Gen,\n    Test.QuickCheck.Gen.Unsafe,\n    Test.QuickCheck.Monadic,\n    Test.QuickCheck.Modifiers,\n    Test.QuickCheck.Property,\n    Test.QuickCheck.Test,\n    Test.QuickCheck.Text,\n    Test.QuickCheck.Poly,\n    Test.QuickCheck.State,\n    Test.QuickCheck.Random,\n    Test.QuickCheck.Exception,\n    Test.QuickCheck.Features\n\n  -- GHC-specific modules.\n  if impl(ghc)\n    Exposed-Modules: Test.QuickCheck.Function\n    Build-depends: transformers >= 0.3, deepseq >= 1.1.0.0\n  else\n    cpp-options: -DNO_TRANSFORMERS -DNO_DEEPSEQ\n\n  if impl(ghc) && flag(templateHaskell)\n    Build-depends: template-haskell >= 2.4\n    if impl(ghc >=8.0)\n      Other-Extensions: TemplateHaskellQuotes\n    else\n      Other-Extensions: TemplateHaskell\n    Exposed-Modules: Test.QuickCheck.All\n  else\n    cpp-options: -DNO_TEMPLATE_HASKELL\n\n  if !impl(ghc >= 7.4)\n    cpp-options: -DNO_CTYPES_CONSTRUCTORS -DNO_FOREIGN_C_USECONDS\n\n  -- The new generics appeared in GHC 7.2...\n  if impl(ghc < 7.2)\n    cpp-options: -DNO_GENERICS\n  -- ...but in 7.2-7.4 it lives in the ghc-prim package.\n  if impl(ghc >= 7.2) && impl(ghc < 7.6)\n    Build-depends: ghc-prim\n\n  -- Safe Haskell appeared in GHC 7.2, but GHC.Generics isn't safe until 7.4.\n  if impl (ghc < 7.4)\n    cpp-options: -DNO_SAFE_HASKELL\n\n  -- random is explicitly Trustworthy since 1.0.1.0\n  -- similar constraint for containers\n  if impl(ghc >= 7.2)\n    Build-depends: random >=1.0.1.0\n  if impl(ghc >= 7.4)\n    Build-depends: containers >=0.4.2.1\n\n  if !impl(ghc >= 7.6)\n      cpp-options: -DNO_POLYKINDS\n\n  if !impl(ghc >= 8.0)\n    cpp-options: -DNO_MONADFAIL\n\n  -- Switch off most optional features on non-GHC systems.\n  if !impl(ghc)\n    -- If your Haskell compiler can cope without some of these, please\n    -- send a message to the QuickCheck mailing list!\n    cpp-options: -DNO_TIMEOUT -DNO_NEWTYPE_DERIVING -DNO_GENERICS\n      -DNO_TEMPLATE_HASKELL -DNO_SAFE_HASKELL -DNO_TYPEABLE -DNO_GADTS\n      -DNO_EXTRA_METHODS_IN_APPLICATIVE -DOLD_RANDOM\n    if !impl(hugs) && !impl(uhc)\n      cpp-options: -DNO_ST_MONAD -DNO_MULTI_PARAM_TYPE_CLASSES\n\n  -- LANGUAGE pragmas don't have any effect in Hugs.\n  if impl(hugs)\n    Default-Extensions: CPP\n\n  if impl(uhc)\n    -- Cabal under UHC needs pointing out all the dependencies of the\n    -- random package.\n    Build-depends: old-time, old-locale\n    -- Plus some bits of the standard library are missing.\n    cpp-options: -DNO_FIXED -DNO_EXCEPTIONS\n\nTest-Suite test-quickcheck\n    type: exitcode-stdio-1.0\n    Default-language: Haskell2010\n    hs-source-dirs:\n        examples\n    main-is: Heap.hs\n    build-depends: base, QuickCheck\n    if !flag(templateHaskell)\n        Buildable: False\n\nTest-Suite test-quickcheck-gcoarbitrary\n    type: exitcode-stdio-1.0\n    Default-language: Haskell2010\n    hs-source-dirs: tests\n    main-is: GCoArbitraryExample.hs\n    build-depends: base, QuickCheck\n    if !flag(templateHaskell) || !impl(ghc >= 7.2)\n        buildable: False\n    if impl(ghc >= 7.2) && impl(ghc < 7.6)\n        build-depends: ghc-prim\n\nTest-Suite test-quickcheck-generators\n    type: exitcode-stdio-1.0\n    Default-language: Haskell2010\n    hs-source-dirs: tests\n    main-is: Generators.hs\n    build-depends: base, QuickCheck\n    if !flag(templateHaskell)\n        Buildable: False\n\nTest-Suite test-quickcheck-gshrink\n    type: exitcode-stdio-1.0\n    Default-language: Haskell2010\n    hs-source-dirs: tests\n    main-is: GShrinkExample.hs\n    build-depends: base, QuickCheck\n    if !flag(templateHaskell) || !impl(ghc >= 7.2)\n        buildable: False\n    if impl(ghc >= 7.2) && impl(ghc < 7.6)\n        build-depends: ghc-prim\n\nTest-Suite test-quickcheck-terminal\n    type: exitcode-stdio-1.0\n    Default-language: Haskell2010\n    hs-source-dirs: tests\n    main-is: Terminal.hs\n    build-depends: base, process, deepseq >= 1.1.0.0, QuickCheck\n    if !flag(templateHaskell) || !impl(ghc >= 7.10)\n        buildable: False\n\nTest-Suite test-quickcheck-monadfix\n    type: exitcode-stdio-1.0\n    Default-language: Haskell2010\n    hs-source-dirs: tests\n    main-is: MonadFix.hs\n    build-depends: base, QuickCheck\n    if !flag(templateHaskell) || !impl(ghc >= 7.10)\n        buildable: False\n\nTest-Suite test-quickcheck-split\n    type: exitcode-stdio-1.0\n    Default-language: Haskell2010\n    hs-source-dirs: tests\n    main-is: Split.hs\n    build-depends: base, QuickCheck\n\nTest-Suite test-quickcheck-misc\n    type: exitcode-stdio-1.0\n    Default-language: Haskell2010\n    hs-source-dirs: tests\n    main-is: Misc.hs\n    build-depends: base, QuickCheck\n    if !flag(templateHaskell) || !impl(ghc >= 7.10)\n        buildable: False\n";
    }