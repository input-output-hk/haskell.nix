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
    flags = { build-tool-depends = true; };
    package = {
      specVersion = "2.0";
      identifier = { name = "genprimopcode"; version = "0.1"; };
      license = "BSD-3-Clause";
      copyright = "XXX";
      maintainer = "XXX";
      author = "XXX";
      homepage = "";
      url = "";
      synopsis = "Generates various files implementing GHC's primitive operations.";
      description = "This utility reads a textual description of GHC's primitive operations\n(@primops.txt.pp@) and produces a number of outputs. These include,\n\n* the @GHC.Prim@ module included in the @ghc-prim@ package.\n* the @GHC.PrimopWrappers@ module included in the @ghc-prim@ package.\n* an LaTeX document describing the primitive operations.";
      buildType = "Simple";
      };
    components = {
      exes = {
        "genprimopcode" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."array" or (errorHandler.buildDepError "array"))
            ];
          build-tools = (pkgs.lib).optionals (flags.build-tool-depends) [
            (hsPkgs.buildPackages.alex.components.exes.alex or (pkgs.buildPackages.alex or (errorHandler.buildToolDepError "alex:alex")))
            (hsPkgs.buildPackages.happy.components.exes.happy or (pkgs.buildPackages.happy or (errorHandler.buildToolDepError "happy:happy")))
            ];
          buildable = true;
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ./.; }
