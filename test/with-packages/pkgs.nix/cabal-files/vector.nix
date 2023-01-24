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
    flags = {
      boundschecks = true;
      unsafechecks = false;
      internalchecks = false;
      wall = false;
      };
    package = {
      specVersion = "1.10";
      identifier = { name = "vector"; version = "0.13.0.0"; };
      license = "BSD-3-Clause";
      copyright = "(c) Roman Leshchinskiy 2008-2012,\nAlexey Kuleshevich 2020-2022,\nAleksey Khudyakov 2020-2022,\nAndrew Lelechenko 2020-2022";
      maintainer = "Haskell Libraries Team <libraries@haskell.org>";
      author = "Roman Leshchinskiy <rl@cse.unsw.edu.au>";
      homepage = "https://github.com/haskell/vector";
      url = "";
      synopsis = "Efficient Arrays";
      description = "\nAn efficient implementation of @Int@-indexed arrays (both mutable\nand immutable), with a powerful loop optimisation framework .\n\nIt is structured as follows:\n\n[\"Data.Vector\"] Boxed vectors of arbitrary types.\n\n[\"Data.Vector.Unboxed\"] Unboxed vectors with an adaptive\nrepresentation based on data type families.\n\n[\"Data.Vector.Storable\"] Unboxed vectors of 'Storable' types.\n\n[\"Data.Vector.Primitive\"] Unboxed vectors of primitive types as\ndefined by the @primitive@ package. \"Data.Vector.Unboxed\" is more\nflexible at no performance cost.\n\n[\"Data.Vector.Generic\"] Generic interface to the vector types.\n\nThere is also a (draft) tutorial on common uses of vector.\n\n* <http://haskell.org/haskellwiki/Numeric_Haskell:_A_Vector_Tutorial>";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."vector-stream" or (errorHandler.buildDepError "vector-stream"))
          ];
        buildable = true;
        };
      tests = {
        "vector-tests-O0" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
            (hsPkgs."base-orphans" or (errorHandler.buildDepError "base-orphans"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            ];
          buildable = true;
          };
        "vector-tests-O2" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
            (hsPkgs."base-orphans" or (errorHandler.buildDepError "base-orphans"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            ];
          buildable = true;
          };
        "vector-doctest" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."doctest" or (errorHandler.buildDepError "doctest"))
            (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            ];
          buildable = (if compiler.isGhc && (compiler.version).lt "8.6"
            then false
            else true) && (if compiler.isGhc && (compiler.version).ge "8.10" && (compiler.isGhc && (compiler.version).lt "8.11")
            then false
            else true) && (if compiler.isGhc && (compiler.version).ge "9.0" && (compiler.isGhc && (compiler.version).lt "9.1")
            then false
            else true) && (if compiler.isGhc && (compiler.version).ge "9.2" && (compiler.isGhc && (compiler.version).lt "9.2.3")
            then false
            else true);
          };
        "vector-inspection" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-inspection-testing" or (errorHandler.buildDepError "tasty-inspection-testing"))
            ];
          buildable = true;
          };
        };
      benchmarks = {
        "algorithms" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-bench" or (errorHandler.buildDepError "tasty-bench"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/vector-0.13.0.0.tar.gz";
      sha256 = "c5d3167d15e12f52e00879ddf304a591672a74e369cc47bc5c7fa1d5a8d15b4f";
      });
    }) // {
    package-description-override = "Name:           vector\r\nVersion:        0.13.0.0\r\nx-revision: 1\r\n-- don't forget to update the changelog file!\r\nLicense:        BSD3\r\nLicense-File:   LICENSE\r\nAuthor:         Roman Leshchinskiy <rl@cse.unsw.edu.au>\r\nMaintainer:     Haskell Libraries Team <libraries@haskell.org>\r\nCopyright:      (c) Roman Leshchinskiy 2008-2012,\r\n                    Alexey Kuleshevich 2020-2022,\r\n                    Aleksey Khudyakov 2020-2022,\r\n                    Andrew Lelechenko 2020-2022\r\n\r\nHomepage:       https://github.com/haskell/vector\r\nBug-Reports:    https://github.com/haskell/vector/issues\r\nCategory:       Data, Data Structures\r\nSynopsis:       Efficient Arrays\r\nDescription:\r\n        .\r\n        An efficient implementation of @Int@-indexed arrays (both mutable\r\n        and immutable), with a powerful loop optimisation framework .\r\n        .\r\n        It is structured as follows:\r\n        .\r\n        [\"Data.Vector\"] Boxed vectors of arbitrary types.\r\n        .\r\n        [\"Data.Vector.Unboxed\"] Unboxed vectors with an adaptive\r\n        representation based on data type families.\r\n        .\r\n        [\"Data.Vector.Storable\"] Unboxed vectors of 'Storable' types.\r\n        .\r\n        [\"Data.Vector.Primitive\"] Unboxed vectors of primitive types as\r\n        defined by the @primitive@ package. \"Data.Vector.Unboxed\" is more\r\n        flexible at no performance cost.\r\n        .\r\n        [\"Data.Vector.Generic\"] Generic interface to the vector types.\r\n        .\r\n        There is also a (draft) tutorial on common uses of vector.\r\n        .\r\n        * <http://haskell.org/haskellwiki/Numeric_Haskell:_A_Vector_Tutorial>\r\n\r\nTested-With:\r\n  GHC == 8.0.2,\r\n  GHC == 8.2.2,\r\n  GHC == 8.4.4,\r\n  GHC == 8.6.5,\r\n  GHC == 8.8.4,\r\n  GHC == 8.10.7,\r\n  GHC == 9.0.1,\r\n  GHC == 9.2.3\r\n\r\nCabal-Version:  >= 1.10\r\nBuild-Type:     Simple\r\n\r\nExtra-Source-Files:\r\n      changelog.md\r\n      README.md\r\n      tests/LICENSE\r\n      tests/Setup.hs\r\n      tests/Main.hs\r\n      internal/GenUnboxTuple.hs\r\n      internal/unbox-tuple-instances\r\n\r\nFlag BoundsChecks\r\n  Description: Enable bounds checking\r\n  Default: True\r\n  Manual: True\r\n\r\nFlag UnsafeChecks\r\n  Description: Enable bounds checking in unsafe operations at the cost of a\r\n               significant performance penalty\r\n  Default: False\r\n  Manual: True\r\n\r\nFlag InternalChecks\r\n  Description: Enable internal consistency checks at the cost of a\r\n               significant performance penalty\r\n  Default: False\r\n  Manual: True\r\n\r\nFlag Wall\r\n  Description: Enable all -Wall warnings\r\n  Default: False\r\n  Manual: True\r\n\r\n\r\nLibrary\r\n  Default-Language: Haskell2010\r\n  Other-Extensions:\r\n        BangPatterns\r\n        CPP\r\n        DeriveDataTypeable\r\n        ExistentialQuantification\r\n        FlexibleContexts\r\n        FlexibleInstances\r\n        GADTs\r\n        KindSignatures\r\n        MagicHash\r\n        MultiParamTypeClasses\r\n        RankNTypes\r\n        ScopedTypeVariables\r\n        StandaloneDeriving\r\n        TypeFamilies\r\n\r\n  Exposed-Modules:\r\n        Data.Vector.Internal.Check\r\n\r\n        Data.Vector.Fusion.Util\r\n        Data.Vector.Fusion.Stream.Monadic\r\n        Data.Vector.Fusion.Bundle.Size\r\n        Data.Vector.Fusion.Bundle.Monadic\r\n        Data.Vector.Fusion.Bundle\r\n\r\n        Data.Vector.Generic.Mutable.Base\r\n        Data.Vector.Generic.Mutable\r\n        Data.Vector.Generic.Base\r\n        Data.Vector.Generic.New\r\n        Data.Vector.Generic\r\n\r\n        Data.Vector.Primitive.Mutable\r\n        Data.Vector.Primitive\r\n\r\n        Data.Vector.Storable.Internal\r\n        Data.Vector.Storable.Mutable\r\n        Data.Vector.Storable\r\n\r\n        Data.Vector.Unboxed.Base\r\n        Data.Vector.Unboxed.Mutable\r\n        Data.Vector.Unboxed\r\n\r\n        Data.Vector.Mutable\r\n        Data.Vector\r\n\r\n  Hs-Source-Dirs:\r\n        src\r\n\r\n  Include-Dirs:\r\n        include, internal\r\n\r\n  Install-Includes:\r\n        vector.h\r\n\r\n  Build-Depends: base >= 4.9 && < 4.18\r\n               , primitive >= 0.6.4.0 && < 0.8\r\n               , deepseq >= 1.1 && < 1.5\r\n               , vector-stream >= 0.1 && < 0.2\r\n\r\n  Ghc-Options: -O2 -Wall\r\n\r\n  if !flag(Wall)\r\n    Ghc-Options: -fno-warn-orphans\r\n\r\n    if impl(ghc >= 8.0) && impl(ghc < 8.1)\r\n      Ghc-Options:   -Wno-redundant-constraints\r\n\r\n  if flag(BoundsChecks)\r\n    cpp-options: -DVECTOR_BOUNDS_CHECKS\r\n\r\n  if flag(UnsafeChecks)\r\n    cpp-options: -DVECTOR_UNSAFE_CHECKS\r\n\r\n  if flag(InternalChecks)\r\n    cpp-options: -DVECTOR_INTERNAL_CHECKS\r\n\r\nsource-repository head\r\n  type:     git\r\n  location: https://github.com/haskell/vector.git\r\n  subdir:   vector\r\n\r\n\r\ntest-suite vector-tests-O0\r\n  Default-Language: Haskell2010\r\n  type: exitcode-stdio-1.0\r\n  Main-Is:  Main.hs\r\n\r\n  other-modules: Boilerplater\r\n                 Tests.Bundle\r\n                 Tests.Move\r\n                 Tests.Vector\r\n                 Tests.Vector.Property\r\n                 Tests.Vector.Boxed\r\n                 Tests.Vector.Storable\r\n                 Tests.Vector.Primitive\r\n                 Tests.Vector.Unboxed\r\n                 Tests.Vector.UnitTests\r\n                 Utilities\r\n\r\n  hs-source-dirs: tests\r\n  Build-Depends: base >= 4.5 && < 5, template-haskell, base-orphans >= 0.6, vector,\r\n                 primitive, random,\r\n                 QuickCheck >= 2.9 && < 2.15, HUnit, tasty,\r\n                 tasty-hunit, tasty-quickcheck,\r\n                 transformers >= 0.2.0.0\r\n\r\n  default-extensions: CPP,\r\n              ScopedTypeVariables,\r\n              PatternGuards,\r\n              MultiParamTypeClasses,\r\n              FlexibleContexts,\r\n              RankNTypes,\r\n              TypeSynonymInstances,\r\n              TypeFamilies,\r\n              TemplateHaskell\r\n\r\n  Ghc-Options: -O0 -threaded\r\n  Ghc-Options: -Wall\r\n\r\n  if !flag(Wall)\r\n    Ghc-Options: -fno-warn-orphans -fno-warn-missing-signatures\r\n    if impl(ghc >= 8.0) && impl(ghc < 8.1)\r\n      Ghc-Options: -Wno-redundant-constraints\r\n\r\n\r\ntest-suite vector-tests-O2\r\n  Default-Language: Haskell2010\r\n  type: exitcode-stdio-1.0\r\n  Main-Is:  Main.hs\r\n\r\n  other-modules: Boilerplater\r\n                 Tests.Bundle\r\n                 Tests.Move\r\n                 Tests.Vector\r\n                 Tests.Vector.Property\r\n                 Tests.Vector.Boxed\r\n                 Tests.Vector.Storable\r\n                 Tests.Vector.Primitive\r\n                 Tests.Vector.Unboxed\r\n                 Tests.Vector.UnitTests\r\n                 Utilities\r\n\r\n  hs-source-dirs: tests\r\n  Build-Depends: base >= 4.5 && < 5, template-haskell, base-orphans >= 0.6, vector,\r\n                 primitive, random,\r\n                 QuickCheck >= 2.9 && < 2.15, HUnit, tasty,\r\n                 tasty-hunit, tasty-quickcheck,\r\n                 transformers >= 0.2.0.0\r\n\r\n  default-extensions: CPP,\r\n              ScopedTypeVariables,\r\n              PatternGuards,\r\n              MultiParamTypeClasses,\r\n              FlexibleContexts,\r\n              RankNTypes,\r\n              TypeSynonymInstances,\r\n              TypeFamilies,\r\n              TemplateHaskell\r\n\r\n  Ghc-Options: -Wall\r\n  Ghc-Options:  -O2 -threaded\r\n  if !flag(Wall)\r\n    Ghc-Options: -fno-warn-orphans -fno-warn-missing-signatures\r\n    if impl(ghc >= 8.0) && impl(ghc < 8.1)\r\n      Ghc-Options: -Wno-redundant-constraints\r\n\r\ntest-suite vector-doctest\r\n  type:             exitcode-stdio-1.0\r\n  main-is:          doctests.hs\r\n  hs-source-dirs:   tests\r\n  default-language: Haskell2010\r\n  -- Older GHC don't support DerivingVia\r\n  if impl(ghc < 8.6)\r\n    buildable: False\r\n  -- GHC 8.10 fails to run doctests for some reason\r\n  if impl(ghc >= 8.10) && impl(ghc < 8.11)\r\n    buildable: False\r\n  -- GHC 9.0 fails to run doctests for some reason too\r\n  if impl(ghc >= 9.0) && impl(ghc < 9.1)\r\n    buildable: False\r\n  -- And GHC 9.2 too\r\n  if impl(ghc >= 9.2) && impl(ghc < 9.2.3)\r\n    buildable: False\r\n  if impl(ghc >= 9.2.3) && impl(ghc < 9.3)\r\n    buildable: True\r\n  build-depends:\r\n        base      -any\r\n      , doctest   >=0.15 && <0.21\r\n      , primitive >= 0.6.4.0 && < 0.8\r\n      , vector    -any\r\n\r\ntest-suite vector-inspection\r\n  type:             exitcode-stdio-1.0\r\n  hs-source-dirs:   tests-inspect\r\n  Ghc-Options:      -Wall\r\n  main-is:          main.hs\r\n  default-language: Haskell2010\r\n  Other-modules:    Inspect\r\n  if impl(ghc >= 8.6)\r\n    Other-modules:  Inspect.DerivingVia\r\n                    Inspect.DerivingVia.OtherFoo\r\n  build-depends:\r\n        base                     -any\r\n      , primitive                >= 0.6.4.0 && < 0.8\r\n      , vector                   -any\r\n      , tasty\r\n      , tasty-inspection-testing >= 0.1\r\n\r\nbenchmark algorithms\r\n  type:             exitcode-stdio-1.0\r\n  main-is:          Main.hs\r\n  hs-source-dirs:   benchmarks\r\n  default-language: Haskell2010\r\n\r\n  build-depends:\r\n        base >= 2 && < 5\r\n      , random >= 1.2\r\n      , tasty\r\n      , tasty-bench >= 0.2.1\r\n      , vector\r\n\r\n  ghc-options: -O2\r\n\r\n  other-modules:\r\n        Algo.MutableSet\r\n        Algo.ListRank\r\n        Algo.Rootfix\r\n        Algo.Leaffix\r\n        Algo.AwShCC\r\n        Algo.HybCC\r\n        Algo.Quickhull\r\n        Algo.Spectral\r\n        Algo.Tridiag\r\n        Algo.FindIndexR\r\n        TestData.ParenTree\r\n        TestData.Graph\r\n";
    }