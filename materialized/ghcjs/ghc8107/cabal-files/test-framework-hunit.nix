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
    flags = { base4 = true; base3 = false; };
    package = {
      specVersion = "1.6";
      identifier = { name = "test-framework-hunit"; version = "0.3.0.2"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Haskell Libraries <libraries@haskell.org>";
      author = "Max Bolingbroke <batterseapower@hotmail.com>";
      homepage = "https://batterseapower.github.io/test-framework/";
      url = "";
      synopsis = "HUnit support for the test-framework package.";
      description = "HUnit support for the test-framework package.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."test-framework" or (errorHandler.buildDepError "test-framework"))
          (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
          (hsPkgs."extensible-exceptions" or (errorHandler.buildDepError "extensible-exceptions"))
          ] ++ (if flags.base3
          then [ (hsPkgs."base" or (errorHandler.buildDepError "base")) ]
          else (pkgs.lib).optional (flags.base4) (hsPkgs."base" or (errorHandler.buildDepError "base")));
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/test-framework-hunit-0.3.0.2.tar.gz";
      sha256 = "95cb8ee02a850b164bfdabdf4dbc839d621361f3ac770ad21ea43a8bde360bf8";
      });
    }