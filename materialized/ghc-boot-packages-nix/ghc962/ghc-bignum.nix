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
    flags = { native = false; ffi = false; gmp = false; check = false; };
    package = {
      specVersion = "2.0";
      identifier = { name = "ghc-bignum"; version = "1.3"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "libraries@haskell.org";
      author = "Sylvain Henry";
      homepage = "";
      url = "";
      synopsis = "GHC BigNum library";
      description = "This package provides the low-level implementation of the standard\n'BigNat', 'Natural' and 'Integer' types.";
      buildType = "Configure";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
          ];
        buildable = (if !flags.native && !flags.gmp && !flags.ffi
          then false
          else true) && (if flags.native && (flags.gmp || flags.ffi)
          then false
          else true) && (if flags.gmp && flags.ffi then false else true);
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ./.; }
