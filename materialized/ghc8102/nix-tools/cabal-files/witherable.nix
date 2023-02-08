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
      specVersion = "2.4";
      identifier = { name = "witherable"; version = "0.4.2"; };
      license = "BSD-3-Clause";
      copyright = "Copyright (c) 2014 Fumiaki Kinoshita";
      maintainer = "Fumiaki Kinoshita <fumiexcel@gmail.com>";
      author = "Fumiaki Kinoshita";
      homepage = "https://github.com/fumieval/witherable";
      url = "";
      synopsis = "filterable traversable";
      description = "A stronger variant of `traverse` which can remove elements and generalised mapMaybe, catMaybes, filter";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."base-orphans" or (errorHandler.buildDepError "base-orphans"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          (hsPkgs."indexed-traversable" or (errorHandler.buildDepError "indexed-traversable"))
          (hsPkgs."indexed-traversable-instances" or (errorHandler.buildDepError "indexed-traversable-instances"))
          ];
        buildable = true;
        };
      tests = {
        "witherable-tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."witherable" or (errorHandler.buildDepError "witherable"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."quickcheck-instances" or (errorHandler.buildDepError "quickcheck-instances"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/witherable-0.4.2.tar.gz";
      sha256 = "790d2bb274283419173bd89104439860675a9410f70f21912973ecd9098b4104";
      });
    }) // {
    package-description-override = "cabal-version:       2.4\nname:                witherable\nversion:             0.4.2\nx-revision:          3\nsynopsis:            filterable traversable\ndescription:         A stronger variant of `traverse` which can remove elements and generalised mapMaybe, catMaybes, filter\nhomepage:            https://github.com/fumieval/witherable\nlicense:             BSD-3-Clause\nlicense-file:        LICENSE\nauthor:              Fumiaki Kinoshita\nmaintainer:          Fumiaki Kinoshita <fumiexcel@gmail.com>\ncopyright:           Copyright (c) 2014 Fumiaki Kinoshita\ncategory:            Data\nbuild-type:          Simple\nextra-source-files:  CHANGELOG.md\ntested-with:         GHC ==8.0.2 || ==8.2.2 || ==8.4.4 || ==8.6.5 || ==8.8.4 || ==8.10.7 || ==9.0.1 || ==9.2.1\n\nsource-repository head\n  type: git\n  location: https://github.com/fumieval/witherable.git\n  subdir: witherable\n\nlibrary\n  exposed-modules:\n    Witherable\n    Data.Witherable\n  build-depends:       base                          >=4.9      && <5,\n                       base-orphans                  >=0.8.4    && <0.9,\n                       containers                    >=0.5.7.1  && <0.7,\n                       hashable                      >=1.2.7.0  && <1.5,\n                       transformers                  >=0.5.2.0  && <0.7,\n                       unordered-containers          >=0.2.12.0 && <0.3,\n                       vector                        >=0.12.2.0 && <0.14,\n                       indexed-traversable           >=0.1.1    && <0.2,\n                       indexed-traversable-instances >=0.1      && <0.2\n  hs-source-dirs:      src\n  ghc-options:         -Wall -Wcompat\n  default-language:    Haskell2010\n\ntest-suite witherable-tests\n  type:                exitcode-stdio-1.0\n  main-is:             tests.hs\n  hs-source-dirs:      tests\n  ghc-options:         -Wall -Wcompat\n  default-language:    Haskell2010\n  build-depends:       base,\n                       witherable,\n                       containers,\n                       hashable,\n                       QuickCheck >=2.14.2,\n                       quickcheck-instances,\n                       tasty,\n                       tasty-quickcheck,\n                       transformers,\n                       unordered-containers,\n                       vector\n";
    }