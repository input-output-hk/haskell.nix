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
      identifier = { name = "primitive"; version = "0.8.0.0"; };
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
          ] ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).lt "9.4") (hsPkgs."data-array-byte" or (errorHandler.buildDepError "data-array-byte"));
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
      url = "http://hackage.haskell.org/package/primitive-0.8.0.0.tar.gz";
      sha256 = "5553c21b4a789f9b591eed69e598cc58484c274af29250e517b5a8bcc62b995f";
      });
    }) // {
    package-description-override = "Cabal-Version:  2.0\nName:           primitive\nVersion:        0.8.0.0\nLicense:        BSD3\nLicense-File:   LICENSE\n\nAuthor:         Roman Leshchinskiy <rl@cse.unsw.edu.au>\nMaintainer:     libraries@haskell.org\nCopyright:      (c) Roman Leshchinskiy 2009-2012\nHomepage:       https://github.com/haskell/primitive\nBug-Reports:    https://github.com/haskell/primitive/issues\nCategory:       Data\nSynopsis:       Primitive memory-related operations\nBuild-Type:     Simple\nDescription:    This package provides various primitive memory-related operations.\n\nExtra-Source-Files: changelog.md\n                    test/*.hs\n                    test/LICENSE\n\nTested-With:\n  GHC == 8.0.2\n  GHC == 8.2.2\n  GHC == 8.4.4\n  GHC == 8.6.5\n  GHC == 8.8.4\n  GHC == 8.10.7\n  GHC == 9.0.2\n  GHC == 9.2.5\n  GHC == 9.4.4\n\nLibrary\n  Default-Language: Haskell2010\n  Default-Extensions:\n        TypeOperators\n  Other-Extensions:\n        BangPatterns, CPP, DeriveDataTypeable,\n        MagicHash, TypeFamilies, UnboxedTuples, UnliftedFFITypes\n\n  Exposed-Modules:\n        Control.Monad.Primitive\n        Data.Primitive\n        Data.Primitive.MachDeps\n        Data.Primitive.Types\n        Data.Primitive.Array\n        Data.Primitive.ByteArray\n        Data.Primitive.PrimArray\n        Data.Primitive.SmallArray\n        Data.Primitive.Ptr\n        Data.Primitive.MutVar\n        Data.Primitive.MVar\n        Data.Primitive.PrimVar\n\n  Other-Modules:\n        Data.Primitive.Internal.Operations\n\n  Build-Depends: base >= 4.9 && < 4.19\n               , deepseq >= 1.1 && < 1.5\n               , transformers >= 0.5 && < 0.7\n               , template-haskell >= 2.11\n\n  if impl(ghc >= 9.2)\n    cpp-options: -DHAVE_KEEPALIVE\n\n  if impl(ghc < 9.4)\n    build-depends: data-array-byte >= 0.1 && < 0.1.1\n\n  Ghc-Options: -O2\n\n  Include-Dirs: cbits\n  Install-Includes: primitive-memops.h\n  includes: primitive-memops.h\n  c-sources: cbits/primitive-memops.c\n  if !os(solaris)\n      cc-options: -ftree-vectorize\n  if arch(i386) || arch(x86_64)\n      cc-options: -msse2\n\ntest-suite test-qc\n  Default-Language: Haskell2010\n  hs-source-dirs: test\n                  test/src\n  main-is: main.hs\n  Other-Modules: PrimLaws\n  type: exitcode-stdio-1.0\n  build-depends: base\n               , base-orphans\n               , ghc-prim\n               , primitive\n               , quickcheck-classes-base >= 0.6 && <0.7\n               , QuickCheck >= 2.13 && < 2.15\n               , tasty ^>= 1.2 || ^>= 1.3 || ^>= 1.4\n               , tasty-quickcheck\n               , tagged\n               , transformers >= 0.5\n               , transformers-compat\n\n  cpp-options: -DHAVE_UNARY_LAWS\n  ghc-options: -O2\n\nbenchmark bench\n  Default-Language: Haskell2010\n  hs-source-dirs: bench\n  main-is: main.hs\n  type: exitcode-stdio-1.0\n  ghc-options: -O2\n  other-modules:\n    Array.Traverse.Closure\n    Array.Traverse.Unsafe\n    ByteArray.Compare\n    PrimArray.Compare\n    PrimArray.Traverse\n  build-depends:\n      base\n    , primitive\n    , deepseq\n    , tasty-bench\n    , transformers >= 0.5\n\nsource-repository head\n  type:     git\n  location: https://github.com/haskell/primitive\n";
    }