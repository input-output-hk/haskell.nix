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
      identifier = { name = "fast-logger"; version = "3.0.3"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Kazu Yamamoto <kazu@iij.ad.jp>";
      author = "Kazu Yamamoto <kazu@iij.ad.jp>";
      homepage = "https://github.com/kazu-yamamoto/logger";
      url = "";
      synopsis = "A fast logging system";
      description = "A fast logging system for Haskell";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          (hsPkgs."auto-update" or (errorHandler.buildDepError "auto-update"))
          (hsPkgs."easy-file" or (errorHandler.buildDepError "easy-file"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."unix-time" or (errorHandler.buildDepError "unix-time"))
          (hsPkgs."unix-compat" or (errorHandler.buildDepError "unix-compat"))
          ] ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).lt "7.8") (hsPkgs."bytestring-builder" or (errorHandler.buildDepError "bytestring-builder"));
        buildable = true;
        };
      tests = {
        "spec" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."fast-logger" or (errorHandler.buildDepError "fast-logger"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            ];
          build-tools = [
            (hsPkgs.buildPackages.hspec-discover.components.exes.hspec-discover or (pkgs.buildPackages.hspec-discover or (errorHandler.buildToolDepError "hspec-discover:hspec-discover")))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/fast-logger-3.0.3.tar.gz";
      sha256 = "5763a0321053ecaba2d1040800bae9988f52b813fb08d5276ea7ce10e3d2f068";
      });
    }