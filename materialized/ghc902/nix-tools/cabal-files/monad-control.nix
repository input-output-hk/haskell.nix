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
      identifier = { name = "monad-control"; version = "1.0.3.1"; };
      license = "BSD-3-Clause";
      copyright = "(c) 2011 Bas van Dijk, Anders Kaseorg";
      maintainer = "Oleg Grenrus <oleg.grenrus@iki.fi>, Bas van Dijk <v.dijk.bas@gmail.com>";
      author = "Bas van Dijk, Anders Kaseorg";
      homepage = "https://github.com/basvandijk/monad-control";
      url = "";
      synopsis = "Lift control operations, like exception catching, through monad transformers";
      description = "This package defines the type class @MonadBaseControl@, a subset of\n@MonadBase@ into which generic control operations such as @catch@ can be\nlifted from @IO@ or any other base monad. Instances are based on monad\ntransformers in @MonadTransControl@, which includes all standard monad\ntransformers in the @transformers@ library except @ContT@.\n\nSee the <http://hackage.haskell.org/package/lifted-base lifted-base>\npackage which uses @monad-control@ to lift @IO@\noperations from the @base@ library (like @catch@ or @bracket@) into any monad\nthat is an instance of @MonadBase@ or @MonadBaseControl@.\n\nNote that this package is a rewrite of Anders Kaseorg's @monad-peel@\nlibrary. The main difference is that this package provides CPS style operators\nand exploits the @RankNTypes@ and @TypeFamilies@ language extensions to\nsimplify and speedup most definitions.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."transformers-base" or (errorHandler.buildDepError "transformers-base"))
          (hsPkgs."transformers-compat" or (errorHandler.buildDepError "transformers-compat"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/monad-control-1.0.3.1.tar.gz";
      sha256 = "ae0baea04d99375ef788140367179994a7178d400a8ce0d9026846546772713c";
      });
    }) // {
    package-description-override = "name:               monad-control\nversion:            1.0.3.1\nsynopsis:\n  Lift control operations, like exception catching, through monad transformers\n\nlicense:            BSD3\nlicense-file:       LICENSE\nauthor:             Bas van Dijk, Anders Kaseorg\nmaintainer:\n  Oleg Grenrus <oleg.grenrus@iki.fi>, Bas van Dijk <v.dijk.bas@gmail.com>\n\ncopyright:          (c) 2011 Bas van Dijk, Anders Kaseorg\nhomepage:           https://github.com/basvandijk/monad-control\nbug-reports:        https://github.com/basvandijk/monad-control/issues\ncategory:           Control\nbuild-type:         Simple\ncabal-version:      1.12\ndescription:\n  This package defines the type class @MonadBaseControl@, a subset of\n  @MonadBase@ into which generic control operations such as @catch@ can be\n  lifted from @IO@ or any other base monad. Instances are based on monad\n  transformers in @MonadTransControl@, which includes all standard monad\n  transformers in the @transformers@ library except @ContT@.\n  .\n  See the <http://hackage.haskell.org/package/lifted-base lifted-base>\n  package which uses @monad-control@ to lift @IO@\n  operations from the @base@ library (like @catch@ or @bracket@) into any monad\n  that is an instance of @MonadBase@ or @MonadBaseControl@.\n  .\n  Note that this package is a rewrite of Anders Kaseorg's @monad-peel@\n  library. The main difference is that this package provides CPS style operators\n  and exploits the @RankNTypes@ and @TypeFamilies@ language extensions to\n  simplify and speedup most definitions.\n\nextra-source-files:\n  CHANGELOG\n  README.markdown\n\ntested-with:\n  GHC ==7.4.2\n   || ==7.6.3\n   || ==7.8.4\n   || ==7.10.3\n   || ==8.0.2\n   || ==8.2.2\n   || ==8.4.4\n   || ==8.6.5\n   || ==8.8.4\n   || ==8.10.4\n   || ==9.0.1\n\n--------------------------------------------------------------------------------\n\nsource-repository head\n  type:     git\n  location: git://github.com/basvandijk/monad-control.git\n\n--------------------------------------------------------------------------------\n\nlibrary\n  default-language: Haskell2010\n  hs-source-dirs:   src\n  ghc-options:      -Wall\n  exposed-modules:  Control.Monad.Trans.Control\n  build-depends:\n      base                 >=4.5   && <5\n    , stm                  >=2.3   && <3\n    , transformers         >=0.2   && <0.7\n    , transformers-base    >=0.4.4 && <0.5\n    , transformers-compat  >=0.3   && <0.8\n";
    }