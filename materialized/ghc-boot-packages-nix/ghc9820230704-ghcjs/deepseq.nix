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
      specVersion = "1.12";
      identifier = { name = "deepseq"; version = "1.4.8.1"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "libraries@haskell.org";
      author = "";
      homepage = "";
      url = "";
      synopsis = "Deep evaluation of data structures";
      description = "This package provides methods for fully evaluating data structures\n(\\\"deep evaluation\\\"). Deep evaluation is often used for adding\nstrictness to a program, e.g. in order to force pending exceptions,\nremove space leaks, or force lazy I/O to happen. It is also useful\nin parallel programs, to ensure pending work does not migrate to the\nwrong thread.\n\nThe primary use of this package is via the 'deepseq' function, a\n\\\"deep\\\" version of 'seq'. It is implemented on top of an 'NFData'\ntypeclass (\\\"Normal Form Data\\\", data structures with no unevaluated\ncomponents) which defines strategies for fully evaluating different\ndata types. See module documentation in \"Control.DeepSeq\" for more\ndetails.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          ] ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).ge "9.0") (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"));
        buildable = true;
        };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."array" or (errorHandler.buildDepError "array"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
            ];
          buildable = true;
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ./.; }
