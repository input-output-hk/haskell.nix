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
    flags = {
      two = false;
      three = false;
      four = false;
      five = false;
      five-three = false;
      mtl = true;
      generic-deriving = true;
      };
    package = {
      specVersion = "1.10";
      identifier = { name = "transformers-compat"; version = "0.6.6"; };
      license = "BSD-3-Clause";
      copyright = "Copyright (C) 2012-2015 Edward A. Kmett";
      maintainer = "Edward A. Kmett <ekmett@gmail.com>";
      author = "Edward A. Kmett";
      homepage = "http://github.com/ekmett/transformers-compat/";
      url = "";
      synopsis = "A small compatibility shim for the transformers library";
      description = "This package includes backported versions of types that were added\nto transformers in transformers 0.3, 0.4, and 0.5 for users who need strict\ntransformers 0.2 or 0.3 compatibility to run on old versions of the\nplatform, but also need those types.\n\nThose users should be able to just depend on @transformers >= 0.2@\nand @transformers-compat >= 0.3@.\n\nNote: missing methods are not supplied, but this at least permits the types to be used.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = ((((((([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          ] ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8.0")) (hsPkgs."fail" or (errorHandler.buildDepError "fail"))) ++ [
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          ]) ++ [
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          ]) ++ [
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          ]) ++ (if flags.three
          then [
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            ] ++ (pkgs.lib).optional (flags.mtl) (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          else [
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            ])) ++ (if flags.two
          then [
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            ] ++ (pkgs.lib).optional (flags.mtl) (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          else [
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            ])) ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).ge "7.2" || flags.generic-deriving) (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))) ++ (pkgs.lib).optionals (flags.generic-deriving) ((pkgs.lib).optional (compiler.isGhc && (compiler.version).lt "8.0" && flags.generic-deriving) (hsPkgs."generic-deriving" or (errorHandler.buildDepError "generic-deriving")));
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/transformers-compat-0.6.6.tar.gz";
      sha256 = "7e2e0251e5e6d28142615a4b950a3fabac9c0b7804b1ec4a4ae985f19519a9f9";
      });
    }