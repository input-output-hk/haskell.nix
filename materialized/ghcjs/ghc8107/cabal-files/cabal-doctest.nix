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
      specVersion = "1.10";
      identifier = { name = "cabal-doctest"; version = "1.0.8"; };
      license = "BSD-3-Clause";
      copyright = "(c) 2017 Oleg Grenrus";
      maintainer = "Oleg Grenrus <oleg.grenrus@iki.fi>";
      author = "Oleg Grenrus <oleg.grenrus@iki.fi>";
      homepage = "https://github.com/phadej/cabal-doctest";
      url = "";
      synopsis = "A Setup.hs helper for doctests running";
      description = "Currently (beginning of 2017), there isn't @cabal doctest@\ncommand. Yet, to properly work doctest needs plenty of configuration.\nThis library provides the common bits for writing custom Setup.hs\nSee <https://github.com/haskell/cabal/issues/2327 Cabal/2327> for the progress\nof @cabal doctest@, i.e. whether this library is obsolete.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."Cabal" or (errorHandler.buildDepError "Cabal"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/cabal-doctest-1.0.8.tar.gz";
      sha256 = "2026a6a87d410202ce091412ca6bc33c5aca787025326b4a3d13425a23392e0e";
      });
    }