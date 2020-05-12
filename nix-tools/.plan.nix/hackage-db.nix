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
    flags = { install-examples = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "hackage-db"; version = "2.1.0"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Peter Simons <simons@cryp.to>";
      author = "Peter Simons, Alexander Altman, Ben James";
      homepage = "https://github.com/peti/hackage-db#readme";
      url = "";
      synopsis = "Access cabal-install's Hackage database via Data.Map";
      description = "This library provides convenient access to the local copy of the Hackage\ndatabase that \\\"cabal update\\\" creates. Check out\n<https://github.com/peti/hackage-db/tree/master/example/> for a collection\nof simple example programs that demonstrate how to use this code.";
      buildType = "Simple";
      isLocal = true;
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."Cabal" or (errorHandler.buildDepError "Cabal"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."tar" or (errorHandler.buildDepError "tar"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."utf8-string" or (errorHandler.buildDepError "utf8-string"))
          ];
        buildable = true;
        };
      exes = {
        "list-known-versions" = {
          depends = (pkgs.lib).optionals (flags.install-examples) [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."Cabal" or (errorHandler.buildDepError "Cabal"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."hackage-db" or (errorHandler.buildDepError "hackage-db"))
            ];
          buildable = if flags.install-examples then true else false;
          };
        "show-meta-data" = {
          depends = (pkgs.lib).optionals (flags.install-examples) [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."Cabal" or (errorHandler.buildDepError "Cabal"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."hackage-db" or (errorHandler.buildDepError "hackage-db"))
            (hsPkgs."utf8-string" or (errorHandler.buildDepError "utf8-string"))
            ];
          buildable = if flags.install-examples then true else false;
          };
        "show-package-versions" = {
          depends = (pkgs.lib).optionals (flags.install-examples) [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."Cabal" or (errorHandler.buildDepError "Cabal"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."hackage-db" or (errorHandler.buildDepError "hackage-db"))
            ];
          buildable = if flags.install-examples then true else false;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/hamishmack/hackage-db.git";
      rev = "3f12730c0d6092efce142cad87264e7b7eb2b05a";
      sha256 = "13nl8swdd3g1rh14f29v6nhnzaxgc8l70vs6hviw8qgdpbxvhs45";
      });
    }