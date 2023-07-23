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
    package-description-override = "cabal-version:       2.4\r\nname:                witherable\r\nversion:             0.4.2\r\nx-revision: 4\r\nsynopsis:            filterable traversable\r\ndescription:         A stronger variant of `traverse` which can remove elements and generalised mapMaybe, catMaybes, filter\r\nhomepage:            https://github.com/fumieval/witherable\r\nlicense:             BSD-3-Clause\r\nlicense-file:        LICENSE\r\nauthor:              Fumiaki Kinoshita\r\nmaintainer:          Fumiaki Kinoshita <fumiexcel@gmail.com>\r\ncopyright:           Copyright (c) 2014 Fumiaki Kinoshita\r\ncategory:            Data\r\nbuild-type:          Simple\r\nextra-source-files:  CHANGELOG.md\r\ntested-with:         GHC ==8.0.2 || ==8.2.2 || ==8.4.4 || ==8.6.5 || ==8.8.4 || ==8.10.7 || ==9.0.1 || ==9.2.1\r\n\r\nsource-repository head\r\n  type: git\r\n  location: https://github.com/fumieval/witherable.git\r\n  subdir: witherable\r\n\r\nlibrary\r\n  exposed-modules:\r\n    Witherable\r\n    Data.Witherable\r\n  build-depends:       base                          >=4.9      && <5,\r\n                       base-orphans                  >=0.8.4    && <0.10,\r\n                       containers                    >=0.5.7.1  && <0.7,\r\n                       hashable                      >=1.2.7.0  && <1.5,\r\n                       transformers                  >=0.5.2.0  && <0.7,\r\n                       unordered-containers          >=0.2.12.0 && <0.3,\r\n                       vector                        >=0.12.2.0 && <0.14,\r\n                       indexed-traversable           >=0.1.1    && <0.2,\r\n                       indexed-traversable-instances >=0.1      && <0.2\r\n  hs-source-dirs:      src\r\n  ghc-options:         -Wall -Wcompat\r\n  default-language:    Haskell2010\r\n\r\ntest-suite witherable-tests\r\n  type:                exitcode-stdio-1.0\r\n  main-is:             tests.hs\r\n  hs-source-dirs:      tests\r\n  ghc-options:         -Wall -Wcompat\r\n  default-language:    Haskell2010\r\n  build-depends:       base,\r\n                       witherable,\r\n                       containers,\r\n                       hashable,\r\n                       QuickCheck >=2.14.2,\r\n                       quickcheck-instances,\r\n                       tasty,\r\n                       tasty-quickcheck,\r\n                       transformers,\r\n                       unordered-containers,\r\n                       vector\r\n";
    }