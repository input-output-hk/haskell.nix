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
      identifier = { name = "newtype-generics"; version = "0.6"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Simon Jakobi <simon.jakobi@gmail.com>";
      author = "Darius Jahandarie, Conor McBride, João Cristóvão, Simon Jakobi";
      homepage = "http://github.com/sjakobi/newtype-generics";
      url = "";
      synopsis = "A typeclass and set of functions for working with newtypes";
      description = "Per Conor McBride, the Newtype typeclass represents the packing and unpacking of a newtype,\nand allows you to operate under that newtype with functions such as ala.\nGenerics support was added in version 0.4, making this package a full replacement\nfor the original newtype package, and a better alternative to newtype-th.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          ];
        buildable = true;
        };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."newtype-generics" or (errorHandler.buildDepError "newtype-generics"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            ];
          build-tools = [
            (hsPkgs.pkgsBuildBuild.hspec-discover.components.exes.hspec-discover or (pkgs.pkgsBuildBuild.hspec-discover or (errorHandler.buildToolDepError "hspec-discover:hspec-discover")))
            ];
          buildable = true;
          };
        };
      benchmarks = {
        "bench" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."gauge" or (errorHandler.buildDepError "gauge"))
            (hsPkgs."newtype-generics" or (errorHandler.buildDepError "newtype-generics"))
            (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/newtype-generics-0.6.tar.gz";
      sha256 = "8a8bace7786b33fe9d356a05b407b41db89f9bad60980d9a664fd33d21af7e11";
      });
    }) // {
    package-description-override = "Name:                newtype-generics\nVersion:             0.6\nSynopsis:            A typeclass and set of functions for working with newtypes\nDescription:         Per Conor McBride, the Newtype typeclass represents the packing and unpacking of a newtype,\n                     and allows you to operate under that newtype with functions such as ala.\n                     Generics support was added in version 0.4, making this package a full replacement\n                     for the original newtype package, and a better alternative to newtype-th.\nLicense:             BSD3\nLicense-file:        LICENSE\nAuthor:              Darius Jahandarie, Conor McBride, João Cristóvão, Simon Jakobi\nMaintainer:          Simon Jakobi <simon.jakobi@gmail.com>\nHomepage:            http://github.com/sjakobi/newtype-generics\nCategory:            Control\nBuild-type:          Simple\nExtra-source-files:  CHANGELOG.md\nCabal-version:       >=1.10\nTested-with:\n  -- GHC==9.0.1,\n  GHC==8.10.3,\n  GHC==8.8.4,\n  GHC==8.6.5,\n  GHC==8.4.4,\n  GHC==8.2.2,\n  GHC==8.0.2\n\nLibrary\n  Exposed-modules:     Control.Newtype.Generics\n  Build-depends:       base >= 4.9 && < 4.16\n                     , transformers < 0.6\n  Ghc-options: -Wall\n  default-language:   Haskell2010\n\nsource-repository head\n  type: git\n  location: https://github.com/sjakobi/newtype-generics\n\ntest-suite test\n  type:               exitcode-stdio-1.0\n  main-is:            main.hs\n  hs-source-dirs:     test\n  other-modules:      Control.NewtypeSpec\n  build-depends:      base\n                    , newtype-generics\n                    , hspec             >= 2.1\n  default-language:   Haskell2010\n  build-tool-depends: hspec-discover:hspec-discover >= 2.1\n\nbenchmark bench\n  type:               exitcode-stdio-1.0\n  main-is:            main.hs\n  hs-source-dirs:     bench\n  build-depends:      base >= 4.7\n                    , gauge\n                    , newtype-generics\n                    , semigroups\n  ghc-options:        -O2\n  default-language:   Haskell2010\n";
    }