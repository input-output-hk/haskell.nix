{ system
  , compiler
  , flags
  , pkgs
  , hsPkgs
  , pkgconfPkgs
  , errorHandler
  , config
  , ... }:
  {
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
    }