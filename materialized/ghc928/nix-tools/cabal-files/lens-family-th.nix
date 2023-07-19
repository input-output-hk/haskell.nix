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
      specVersion = "1.10";
      identifier = { name = "lens-family-th"; version = "0.5.2.1"; };
      license = "BSD-3-Clause";
      copyright = "(c) Dan Burton 2012-2020";
      maintainer = "danburton.email@gmail.com";
      author = "Dan Burton";
      homepage = "http://github.com/DanBurton/lens-family-th#readme";
      url = "";
      synopsis = "Generate lens-family style lenses";
      description = "(see README.md)";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          ];
        buildable = true;
        };
      tests = {
        "lens-family-th-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."lens-family-th" or (errorHandler.buildDepError "lens-family-th"))
            (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/lens-family-th-0.5.2.1.tar.gz";
      sha256 = "b01d6bb9325fa1729325ebc94b70ccf7957bbe71cb11367b3acd791a143213f1";
      });
    }) // {
    package-description-override = "name:                lens-family-th\nversion:             0.5.2.1\nsynopsis:            Generate lens-family style lenses\n\ndescription:         (see README.md)\n\nlicense:             BSD3\nlicense-file:        LICENSE\nauthor:              Dan Burton\ncopyright:           (c) Dan Burton 2012-2020\n\nhomepage:            http://github.com/DanBurton/lens-family-th#readme\nbug-reports:         http://github.com/DanBurton/lens-family-th/issues\nmaintainer:          danburton.email@gmail.com\n\ncategory:            Data\nbuild-type:          Simple\ncabal-version:       >=1.10\n\nextra-source-files:  README.md\n\nlibrary\n  default-language:  Haskell2010\n  hs-source-dirs:    src\n  exposed-modules:   Lens.Family.TH\n                   , Lens.Family2.TH\n                   , Lens.Family.THCore\n  build-depends:     base >= 4.9 && < 5\n                   , template-haskell >= 2.11 && < 2.19\n\ntest-suite lens-family-th-test\n  default-language:  Haskell2010\n  type:              exitcode-stdio-1.0\n  hs-source-dirs:    test\n  main-is:           Test.hs\n  build-depends:     base\n                   , hspec\n                   , transformers\n                   , lens-family-th\n                   , template-haskell\n\nsource-repository head\n  type:      git\n  location:  git://github.com/DanBurton/lens-family-th.git\n";
    }