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
      identifier = { name = "Includes3"; version = "0.1.0.0"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "ezyang@cs.stanford.edu";
      author = "Edward Z. Yang";
      homepage = "";
      url = "";
      synopsis = "";
      description = "";
      buildType = "Simple";
      isLocal = true;
      };
    components = {
      sublibs = {
        "sigs" = {
          depends = [ (hsPkgs."base" or (errorHandler.buildDepError "base")) ];
          buildable = true;
          };
        "indef" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."Includes3".components.sublibs.sigs or (errorHandler.buildDepError "Includes3:sigs"))
            ];
          buildable = true;
          };
        "indef+6YmfnARghEC3uazvHVLJ9b" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."Includes3".components.sublibs.sigs or (errorHandler.buildDepError "Includes3:sigs"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."Includes3".components.sublibs.indef or (errorHandler.buildDepError "Includes3:indef"))
            ];
          buildable = true;
          instantiatedWith = [ "Data.Map=containers-0.6.5.1:Data.Map" ];
          };
        };
      exes = {
        "exe" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."Includes3".components.sublibs."indef+6YmfnARghEC3uazvHVLJ9b" or (errorHandler.buildDepError "Includes3:\"indef+6YmfnARghEC3uazvHVLJ9b\""))
            ];
          buildable = true;
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault .././../.; }