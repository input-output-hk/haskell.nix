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
      identifier = { name = "repline"; version = "0.4.2.0"; };
      license = "MIT";
      copyright = "2014-2022 Stephen Diehl";
      maintainer = "stephen.m.diehl@gmail.com";
      author = "Stephen Diehl";
      homepage = "https://github.com/sdiehl/repline";
      url = "";
      synopsis = "Haskeline wrapper for GHCi-like REPL interfaces.";
      description = "Haskeline wrapper for GHCi-like REPL interfaces. Composable with normal mtl transformers.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
          (hsPkgs."haskeline" or (errorHandler.buildDepError "haskeline"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."process" or (errorHandler.buildDepError "process"))
          ] ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8.0")) (hsPkgs."fail" or (errorHandler.buildDepError "fail"));
        buildable = true;
        };
      tests = {
        "prefix" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."repline" or (errorHandler.buildDepError "repline"))
            ];
          buildable = true;
          };
        "simple" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."process" or (errorHandler.buildDepError "process"))
            (hsPkgs."repline" or (errorHandler.buildDepError "repline"))
            ];
          buildable = true;
          };
        "stateful" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."repline" or (errorHandler.buildDepError "repline"))
            ];
          buildable = true;
          };
        "multiline" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."process" or (errorHandler.buildDepError "process"))
            (hsPkgs."repline" or (errorHandler.buildDepError "repline"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/repline-0.4.2.0.tar.gz";
      sha256 = "6181b3f106b4059eb1c86c4a442083c29335a1d9a3e7cfffecb962ec05b08d5a";
      });
    }