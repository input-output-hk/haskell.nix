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
      identifier = {
        name = "indexed-traversable-instances";
        version = "0.1.1.1";
        };
      license = "BSD-2-Clause";
      copyright = "";
      maintainer = "Oleg Grenrus <oleg.grenrus@iki.fi>";
      author = "Edward Kmett";
      homepage = "";
      url = "";
      synopsis = "More instances of FunctorWithIndex, FoldableWithIndex, TraversableWithIndex";
      description = "This package provides extra instances for type-classes in the [indexed-traversable](https://hackage.haskell.org/package/indexed-traversable) package.\n\nThe intention is to keep this package minimal;\nit provides instances that formely existed in @lens@ or @optics-extra@.\nWe recommend putting other instances directly into their defining packages.\nThe @indexed-traversable@ package is light, having only GHC boot libraries\nas its dependencies.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."indexed-traversable" or (errorHandler.buildDepError "indexed-traversable"))
          (hsPkgs."OneTuple" or (errorHandler.buildDepError "OneTuple"))
          (hsPkgs."tagged" or (errorHandler.buildDepError "tagged"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          ];
        buildable = true;
        };
      tests = {
        "safe" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."indexed-traversable" or (errorHandler.buildDepError "indexed-traversable"))
            (hsPkgs."indexed-traversable-instances" or (errorHandler.buildDepError "indexed-traversable-instances"))
            ];
          buildable = true;
          };
        "indexed-tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."indexed-traversable" or (errorHandler.buildDepError "indexed-traversable"))
            (hsPkgs."indexed-traversable-instances" or (errorHandler.buildDepError "indexed-traversable-instances"))
            (hsPkgs."OneTuple" or (errorHandler.buildDepError "OneTuple"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."quickcheck-instances" or (errorHandler.buildDepError "quickcheck-instances"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/indexed-traversable-instances-0.1.1.1.tar.gz";
      sha256 = "895ee5063d59e2b58930ca6fa54885bb11f19c8b7c7ba6c7b014f9431cdcc0b0";
      });
    }) // {
    package-description-override = "cabal-version:      1.12\nname:               indexed-traversable-instances\nversion:            0.1.1.1\nbuild-type:         Simple\nlicense:            BSD2\nlicense-file:       LICENSE\ncategory:           Data\nmaintainer:         Oleg Grenrus <oleg.grenrus@iki.fi>\nauthor:             Edward Kmett\nsynopsis:\n  More instances of FunctorWithIndex, FoldableWithIndex, TraversableWithIndex\n\ndescription:\n  This package provides extra instances for type-classes in the [indexed-traversable](https://hackage.haskell.org/package/indexed-traversable) package.\n  .\n  The intention is to keep this package minimal;\n  it provides instances that formely existed in @lens@ or @optics-extra@.\n  We recommend putting other instances directly into their defining packages.\n  The @indexed-traversable@ package is light, having only GHC boot libraries\n  as its dependencies.\n\nextra-source-files: Changelog.md\ntested-with:\n  GHC ==7.4.2\n   || ==7.6.3\n   || ==7.8.4\n   || ==7.10.3\n   || ==8.0.2\n   || ==8.2.2\n   || ==8.4.4\n   || ==8.6.5\n   || ==8.8.4\n   || ==8.10.7\n   || ==9.0.2\n   || ==9.2.4\n   || ==9.4.1\n\nsource-repository head\n  type:     git\n  location: https://github.com/haskellari/indexed-traversable\n  subdir:   indexed-traversable-instances\n\nlibrary\n  default-language: Haskell2010\n  ghc-options:      -Wall\n  hs-source-dirs:   src\n  build-depends:\n      base                  >=4.5      && <4.18\n    , indexed-traversable   >=0.1      && <0.2\n    , OneTuple              >=0.3      && <0.4\n    , tagged                >=0.8.6    && <0.9\n    , unordered-containers  >=0.2.8.0  && <0.3\n    , vector                >=0.12.1.2 && <0.14\n\n  exposed-modules:  Data.Functor.WithIndex.Instances\n\ntest-suite safe\n  type:             exitcode-stdio-1.0\n  default-language: Haskell2010\n  ghc-options:      -Wall\n  hs-source-dirs:   tests\n  main-is:          safe.hs\n  build-depends:\n      base\n    , indexed-traversable\n    , indexed-traversable-instances\n\ntest-suite indexed-tests\n  type:             exitcode-stdio-1.0\n  default-language: Haskell2010\n  ghc-options:      -Wall\n  hs-source-dirs:   tests\n  main-is:          main.hs\n  build-depends:\n      base\n    , containers\n    , indexed-traversable\n    , indexed-traversable-instances\n    , OneTuple\n    , transformers\n    , unordered-containers\n    , vector\n\n  build-depends:\n      QuickCheck            >=2.14.2   && <2.15\n    , quickcheck-instances  >=0.3.26   && <0.4\n    , tasty                 >=1.2.3    && <1.5\n    , tasty-quickcheck      >=0.10.1.1 && <0.11\n";
    }