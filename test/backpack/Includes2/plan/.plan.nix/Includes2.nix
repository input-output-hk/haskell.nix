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
      identifier = { name = "Includes2"; version = "0.1.0.0"; };
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
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."Includes2".components.sublibs.mysql or (errorHandler.buildDepError "Includes2:mysql"))
          (hsPkgs."Includes2".components.sublibs.postgresql or (errorHandler.buildDepError "Includes2:postgresql"))
          (hsPkgs."Includes2".components.sublibs."mylib+3gY9SyjX86dBypHcOaev1n" or (errorHandler.buildDepError "Includes2:\"mylib+3gY9SyjX86dBypHcOaev1n\""))
          (hsPkgs."Includes2".components.sublibs."mylib+BfeYWT4IxdwEBRldDzV3Na" or (errorHandler.buildDepError "Includes2:\"mylib+BfeYWT4IxdwEBRldDzV3Na\""))
          ];
        buildable = true;
        };
      sublibs = {
        "mylib" = {
          depends = [ (hsPkgs."base" or (errorHandler.buildDepError "base")) ];
          buildable = true;
          };
        "mysql" = {
          depends = [ (hsPkgs."base" or (errorHandler.buildDepError "base")) ];
          buildable = true;
          };
        "postgresql" = {
          depends = [ (hsPkgs."base" or (errorHandler.buildDepError "base")) ];
          buildable = true;
          };
        "mylib+BfeYWT4IxdwEBRldDzV3Na" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."Includes2".components.sublibs.mylib or (errorHandler.buildDepError "Includes2:mylib"))
            (hsPkgs."Includes2".components.sublibs.postgresql or (errorHandler.buildDepError "Includes2:postgresql"))
            ];
          buildable = true;
          instantiatedWith = [
            "Database=Includes2-0.1.0.0-inplace-postgresql:Database.PostgreSQL"
            ];
          };
        "mylib+3gY9SyjX86dBypHcOaev1n" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."Includes2".components.sublibs.mylib or (errorHandler.buildDepError "Includes2:mylib"))
            (hsPkgs."Includes2".components.sublibs.mysql or (errorHandler.buildDepError "Includes2:mysql"))
            ];
          buildable = true;
          instantiatedWith = [
            "Database=Includes2-0.1.0.0-inplace-mysql:Database.MySQL"
            ];
          };
        };
      exes = {
        "exe" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."Includes2" or (errorHandler.buildDepError "Includes2"))
            ];
          buildable = true;
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault .././../.; }