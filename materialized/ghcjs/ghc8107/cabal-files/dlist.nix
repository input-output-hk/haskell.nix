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
      identifier = { name = "dlist"; version = "0.8.0.8"; };
      license = "BSD-3-Clause";
      copyright = "2006-2009 Don Stewart, 2013-2019 Sean Leather";
      maintainer = "Sean Leather <sean.leather@gmail.com>";
      author = "Don Stewart";
      homepage = "https://github.com/spl/dlist";
      url = "";
      synopsis = "Difference lists";
      description = "Difference lists are a list-like type supporting O(1) append. This is\nparticularly useful for efficient logging and pretty printing (e.g. with the\nWriter monad), where list append quickly becomes too expensive.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          ];
        buildable = true;
        };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."dlist" or (errorHandler.buildDepError "dlist"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."Cabal" or (errorHandler.buildDepError "Cabal"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/dlist-0.8.0.8.tar.gz";
      sha256 = "7129cf18068d3384e305708a10426ab8f573bee1030b023a114f45f1d0ec496d";
      });
    }