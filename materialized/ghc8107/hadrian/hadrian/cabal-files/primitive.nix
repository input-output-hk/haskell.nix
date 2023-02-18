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
      specVersion = "2.0";
      identifier = { name = "primitive"; version = "0.7.4.0"; };
      license = "BSD-3-Clause";
      copyright = "(c) Roman Leshchinskiy 2009-2012";
      maintainer = "libraries@haskell.org";
      author = "Roman Leshchinskiy <rl@cse.unsw.edu.au>";
      homepage = "https://github.com/haskell/primitive";
      url = "";
      synopsis = "Primitive memory-related operations";
      description = "This package provides various primitive memory-related operations.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          ];
        buildable = true;
        };
      tests = {
        "test-qc" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base-orphans" or (errorHandler.buildDepError "base-orphans"))
            (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
            (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
            (hsPkgs."quickcheck-classes-base" or (errorHandler.buildDepError "quickcheck-classes-base"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."tagged" or (errorHandler.buildDepError "tagged"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."transformers-compat" or (errorHandler.buildDepError "transformers-compat"))
            ];
          buildable = true;
          };
        };
      benchmarks = {
        "bench" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."tasty-bench" or (errorHandler.buildDepError "tasty-bench"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/primitive-0.7.4.0.tar.gz";
      sha256 = "5b2d6dc2812eb2f6a115f05fcbe3e723d3aeff7894b012c617e075130581add5";
      });
    }) // {
    package-description-override = "Cabal-Version:  2.0\r\nName:           primitive\r\nVersion:        0.7.4.0\r\nx-revision: 1\r\nLicense:        BSD3\r\nLicense-File:   LICENSE\r\n\r\nAuthor:         Roman Leshchinskiy <rl@cse.unsw.edu.au>\r\nMaintainer:     libraries@haskell.org\r\nCopyright:      (c) Roman Leshchinskiy 2009-2012\r\nHomepage:       https://github.com/haskell/primitive\r\nBug-Reports:    https://github.com/haskell/primitive/issues\r\nCategory:       Data\r\nSynopsis:       Primitive memory-related operations\r\nBuild-Type:     Simple\r\nDescription:    This package provides various primitive memory-related operations.\r\n\r\nExtra-Source-Files: changelog.md\r\n                    test/*.hs\r\n                    test/LICENSE\r\n\r\nTested-With:\r\n  GHC == 8.0.2,\r\n  GHC == 8.2.2,\r\n  GHC == 8.4.4,\r\n  GHC == 8.6.5,\r\n  GHC == 8.8.4,\r\n  GHC == 8.10.7\r\n\r\nLibrary\r\n  Default-Language: Haskell2010\r\n  Other-Extensions:\r\n        BangPatterns, CPP, DeriveDataTypeable,\r\n        MagicHash, TypeFamilies, UnboxedTuples, UnliftedFFITypes\r\n\r\n  Exposed-Modules:\r\n        Control.Monad.Primitive\r\n        Data.Primitive\r\n        Data.Primitive.MachDeps\r\n        Data.Primitive.Types\r\n        Data.Primitive.Array\r\n        Data.Primitive.ByteArray\r\n        Data.Primitive.PrimArray\r\n        Data.Primitive.SmallArray\r\n        Data.Primitive.Ptr\r\n        Data.Primitive.MutVar\r\n        Data.Primitive.MVar\r\n\r\n  Other-Modules:\r\n        Data.Primitive.Internal.Operations\r\n\r\n  Build-Depends: base >= 4.9 && < 4.19\r\n               , deepseq >= 1.1 && < 1.5\r\n               , transformers >= 0.5 && < 0.7\r\n               , template-haskell >= 2.11\r\n\r\n  Ghc-Options: -O2\r\n\r\n  Include-Dirs: cbits\r\n  Install-Includes: primitive-memops.h\r\n  includes: primitive-memops.h\r\n  c-sources: cbits/primitive-memops.c\r\n  if !os(solaris)\r\n      cc-options: -ftree-vectorize\r\n  if arch(i386) || arch(x86_64)\r\n      cc-options: -msse2\r\n\r\ntest-suite test-qc\r\n  Default-Language: Haskell2010\r\n  hs-source-dirs: test\r\n                  test/src\r\n  main-is: main.hs\r\n  Other-Modules: PrimLaws\r\n  type: exitcode-stdio-1.0\r\n  build-depends: base\r\n               , base-orphans\r\n               , ghc-prim\r\n               , primitive\r\n               , quickcheck-classes-base >= 0.6 && <0.7\r\n               , QuickCheck >= 2.13 && < 2.15\r\n               , tasty ^>= 1.2 || ^>= 1.3 || ^>= 1.4\r\n               , tasty-quickcheck\r\n               , tagged\r\n               , transformers >= 0.5\r\n               , transformers-compat\r\n\r\n  cpp-options: -DHAVE_UNARY_LAWS\r\n  ghc-options: -O2\r\n\r\nbenchmark bench\r\n  Default-Language: Haskell2010\r\n  hs-source-dirs: bench\r\n  main-is: main.hs\r\n  type: exitcode-stdio-1.0\r\n  ghc-options: -O2\r\n  other-modules:\r\n    Array.Traverse.Closure\r\n    Array.Traverse.Unsafe\r\n    ByteArray.Compare\r\n    PrimArray.Compare\r\n    PrimArray.Traverse\r\n  build-depends:\r\n      base\r\n    , primitive\r\n    , deepseq\r\n    , tasty-bench\r\n    , transformers >= 0.5\r\n\r\nsource-repository head\r\n  type:     git\r\n  location: https://github.com/haskell/primitive\r\n";
    }