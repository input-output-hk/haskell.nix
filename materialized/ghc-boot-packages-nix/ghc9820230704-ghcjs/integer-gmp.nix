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
      specVersion = "2.0";
      identifier = { name = "integer-gmp"; version = "1.1"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "hvr@gnu.org";
      author = "Herbert Valerio Riedel";
      homepage = "https://www.haskell.org/ghc/";
      url = "";
      synopsis = "Integer library based on GMP";
      description = "This package used to provide an implementation of the standard 'Integer'\ntype based on the\n<http://gmplib.org/ GNU Multiple Precision Arithmetic Library (GMP)>.\n\nIt is now deprecated in favor of the 'ghc-bignum' package.\n\nIts purpose is to provide backward compatibility for codes directly\ndepending on the `integer-gmp` package.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
          (hsPkgs."ghc-bignum" or (errorHandler.buildDepError "ghc-bignum"))
          ];
        buildable = true;
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ./.; }
