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
      specVersion = "3.0";
      identifier = { name = "ghc-heap"; version = "9.10.2"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "libraries@haskell.org";
      author = "";
      homepage = "";
      url = "";
      synopsis = "Functions for walking GHC's heap";
      description = "This package provides functions for walking the GHC heap data structures\nand retrieving information about those data structures.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
          (hsPkgs."rts" or (errorHandler.buildDepError "rts"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
        ] ++ pkgs.lib.optional (compiler.isGhc && compiler.version.ge "9.9") (hsPkgs."ghc-internal" or (errorHandler.buildDepError "ghc-internal"));
        buildable = true;
      };
    };
  } // rec { src = pkgs.lib.mkDefault ./.; }
