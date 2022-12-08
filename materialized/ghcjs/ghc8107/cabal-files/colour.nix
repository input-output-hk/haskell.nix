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
      identifier = { name = "colour"; version = "2.3.5"; };
      license = "MIT";
      copyright = "";
      maintainer = "Russell O'Connor <roconnor@theorem.ca>";
      author = "Russell O'Connor";
      homepage = "http://www.haskell.org/haskellwiki/Colour";
      url = "";
      synopsis = "A model for human colour/color perception";
      description = "This package provides a data type for colours and transparency.\nColours can be blended and composed.\nVarious colour spaces are supported.\nA module of colour names (\"Data.Colour.Names\") is provided.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [ (hsPkgs."base" or (errorHandler.buildDepError "base")) ];
        buildable = true;
        };
      tests = {
        "test-colour" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."test-framework" or (errorHandler.buildDepError "test-framework"))
            (hsPkgs."test-framework-quickcheck2" or (errorHandler.buildDepError "test-framework-quickcheck2"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/colour-2.3.5.tar.gz";
      sha256 = "3b8d471979617dce7c193523743c9782df63433d8e87e3ef6d97922e0da104e7";
      });
    }