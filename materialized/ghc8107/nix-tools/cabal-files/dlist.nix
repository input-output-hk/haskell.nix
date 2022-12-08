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
    flags = { werror = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "dlist"; version = "1.0"; };
      license = "BSD-3-Clause";
      copyright = "2006-2009 Don Stewart, 2013-2020 Sean Leather, 2017-2020 Oleg Grenrus, contributors";
      maintainer = "Sean Leather <sean.leather@gmail.com>";
      author = "Don Stewart";
      homepage = "https://github.com/spl/dlist";
      url = "";
      synopsis = "Difference lists";
      description = "List-like types supporting O(1) append and snoc operations.";
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
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/dlist-1.0.tar.gz";
      sha256 = "173d637328bb173fcc365f30d29ff4a94292a1e0e5558aeb3dfc11de81510115";
      });
    }