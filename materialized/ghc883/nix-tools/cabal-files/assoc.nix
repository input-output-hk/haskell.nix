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
      identifier = { name = "assoc"; version = "1.0.2"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Oleg Grenrus <oleg.grenrus@iki.fi>";
      author = "Oleg Grenrus <oleg.grenrus@iki.fi>";
      homepage = "";
      url = "";
      synopsis = "swap and assoc: Symmetric and Semigroupy Bifunctors";
      description = "Provides generalisations of\n@swap :: (a,b) -> (b,a)@ and\n@assoc :: ((a,b),c) -> (a,(b,c))@\nto\n@Bifunctor@s supporting similar operations (e.g. @Either@, @These@).";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bifunctors" or (errorHandler.buildDepError "bifunctors"))
          (hsPkgs."tagged" or (errorHandler.buildDepError "tagged"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/assoc-1.0.2.tar.gz";
      sha256 = "d8988dc6e8718c7a3456515b769c9336aeeec730cf86fc5175247969ff8f144f";
      });
    }) // {
    package-description-override = "cabal-version:      1.12\nname:               assoc\nversion:            1.0.2\nx-revision:         3\nlicense:            BSD3\nlicense-file:       LICENSE\nsynopsis:           swap and assoc: Symmetric and Semigroupy Bifunctors\ncategory:           Data\ndescription:\n  Provides generalisations of\n  @swap :: (a,b) -> (b,a)@ and\n  @assoc :: ((a,b),c) -> (a,(b,c))@\n  to\n  @Bifunctor@s supporting similar operations (e.g. @Either@, @These@).\n\nauthor:             Oleg Grenrus <oleg.grenrus@iki.fi>\nmaintainer:         Oleg Grenrus <oleg.grenrus@iki.fi>\nbuild-type:         Simple\nextra-source-files: CHANGELOG.md\ntested-with:\n    GHC ==7.0.4\n     || ==7.2.2\n     || ==7.4.2\n     || ==7.6.3\n     || ==7.8.4\n     || ==7.10.3\n     || ==8.0.2\n     || ==8.2.2\n     || ==8.4.4\n     || ==8.6.5\n     || ==8.8.4\n     || ==8.10.4\n     || ==9.0.2\n     || ==9.2.4\n     || ==9.4.1\n  , GHCJS ==8.4\n\nsource-repository head\n  type:     git\n  location: https://github.com/phadej/assoc.git\n\nlibrary\n  default-language: Haskell2010\n  hs-source-dirs:   src\n  build-depends:\n      base        >=4.3   && <4.18\n    , bifunctors  >=5.5.5 && <5.6\n    , tagged      >=0.8.6 && <0.9\n\n  exposed-modules:\n    Data.Bifunctor.Assoc\n    Data.Bifunctor.Swap\n\n  other-extensions: TypeFamilies\n";
    }