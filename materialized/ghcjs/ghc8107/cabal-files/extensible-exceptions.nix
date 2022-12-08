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
      specVersion = "1.2";
      identifier = { name = "extensible-exceptions"; version = "0.1.1.4"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "libraries@haskell.org";
      author = "";
      homepage = "";
      url = "";
      synopsis = "Extensible exceptions";
      description = "This package provides extensible exceptions for both new and\nold versions of GHC (i.e., < 6.10).";
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
      url = "http://hackage.haskell.org/package/extensible-exceptions-0.1.1.4.tar.gz";
      sha256 = "6ce5e8801760385a408dab71b53550f87629e661b260bdc2cd41c6a439b6e388";
      });
    }