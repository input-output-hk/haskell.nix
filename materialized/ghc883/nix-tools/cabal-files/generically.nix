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
      identifier = { name = "generically"; version = "0.1"; };
      license = "BSD-3-Clause";
      copyright = "2022 Oleg Grenrus";
      maintainer = "Oleg Grenrus <oleg.grenrus@iki.fi>";
      author = "Oleg Grenrus";
      homepage = "";
      url = "";
      synopsis = "Generically newtype to use with DerivingVia";
      description = "This is a compatibility package as @Generically@ and @Generically1@ newtypes\nare available since @base-4.17@ in 'GHC.Generics'.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [ (hsPkgs."base" or (errorHandler.buildDepError "base")) ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/generically-0.1.tar.gz";
      sha256 = "d5b72880e1d00b24b6c221978d060bea620acc412cdcb62521f3016cbd3f2b71";
      });
    }) // {
    package-description-override = "cabal-version:      1.12\nname:               generically\nversion:            0.1\nsynopsis:           Generically newtype to use with DerivingVia\ndescription:\n  This is a compatibility package as @Generically@ and @Generically1@ newtypes\n  are available since @base-4.17@ in 'GHC.Generics'.\n\nbug-reports:        https://github.com/haskell-compat/generically/issues\nauthor:             Oleg Grenrus\nmaintainer:         Oleg Grenrus <oleg.grenrus@iki.fi>\ncopyright:          2022 Oleg Grenrus\nlicense:            BSD3\nlicense-file:       LICENSE\ncategory:           Generics\nbuild-type:         Simple\nextra-source-files: CHANGELOG.md\ntested-with:\n  GHC ==8.0.2\n   || ==8.2.2\n   || ==8.4.4\n   || ==8.6.5\n   || ==8.8.4\n   || ==8.10.7\n   || ==9.0.2\n   || ==9.2.3\n   || ==9.4.1\n\nsource-repository head\n  type:     git\n  location: https://github.com/haskell-compat/generically.git\n\nlibrary\n  default-language: Haskell2010\n  build-depends:    base >=4.9 && <4.18\n  hs-source-dirs:   src\n  exposed-modules:  GHC.Generics.Generically\n";
    }