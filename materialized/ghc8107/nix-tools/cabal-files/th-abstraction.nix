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
      identifier = { name = "th-abstraction"; version = "0.4.5.0"; };
      license = "ISC";
      copyright = "2017 Eric Mertens";
      maintainer = "emertens@gmail.com";
      author = "Eric Mertens";
      homepage = "https://github.com/glguy/th-abstraction";
      url = "";
      synopsis = "Nicer interface for reified information about data types";
      description = "This package normalizes variations in the interface for\ninspecting datatype information via Template Haskell\nso that packages and support a single, easier to use\ninformational datatype while supporting many versions\nof Template Haskell.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          ];
        buildable = true;
        };
      tests = {
        "unit-tests" = {
          depends = [
            (hsPkgs."th-abstraction" or (errorHandler.buildDepError "th-abstraction"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/th-abstraction-0.4.5.0.tar.gz";
      sha256 = "00d5e24f247e328bd9898d5af5915c1e86b134b4d40baa680258635f95031526";
      });
    }