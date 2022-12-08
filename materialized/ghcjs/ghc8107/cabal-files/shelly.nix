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
    flags = { lifted = false; build-examples = false; };
    package = {
      specVersion = "1.8";
      identifier = { name = "shelly"; version = "1.9.0"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Greg Weber <greg@gregweber.info>";
      author = "Greg Weber, Petr Rockai";
      homepage = "https://github.com/yesodweb/Shelly.hs";
      url = "";
      synopsis = "shell-like (systems) programming in Haskell";
      description = "Shelly provides convenient systems programming in Haskell,\nsimilar in spirit to POSIX shells. Shelly:\n\n* is aimed at convenience and getting things done rather than\nbeing a demonstration of elegance.\n\n* has detailed and useful error messages\n\n* maintains its own environment, making it thread-safe.\n\n* is modern, using Text filepath/directory\n\nShelly is originally forked from the Shellish package.\n\nSee the shelly-extra package for additional functionality.\n\nAn overview is available in the README: <https://github.com/yesodweb/Shelly.hs/blob/master/README.md>";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."process" or (errorHandler.buildDepError "process"))
          (hsPkgs."unix-compat" or (errorHandler.buildDepError "unix-compat"))
          (hsPkgs."unix" or (errorHandler.buildDepError "unix"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."monad-control" or (errorHandler.buildDepError "monad-control"))
          (hsPkgs."lifted-base" or (errorHandler.buildDepError "lifted-base"))
          (hsPkgs."lifted-async" or (errorHandler.buildDepError "lifted-async"))
          (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
          (hsPkgs."enclosed-exceptions" or (errorHandler.buildDepError "enclosed-exceptions"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."async" or (errorHandler.buildDepError "async"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."transformers-base" or (errorHandler.buildDepError "transformers-base"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          ] ++ [ (hsPkgs."base" or (errorHandler.buildDepError "base")) ];
        buildable = true;
        };
      exes = {
        "drain" = {
          depends = (pkgs.lib).optionals (flags.build-examples) [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."shelly" or (errorHandler.buildDepError "shelly"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            ];
          buildable = if flags.build-examples then true else false;
          };
        "run-handles" = {
          depends = (pkgs.lib).optionals (flags.build-examples) [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."shelly" or (errorHandler.buildDepError "shelly"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            ];
          buildable = if flags.build-examples then true else false;
          };
        "Color" = {
          depends = (pkgs.lib).optionals (flags.build-examples) [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."process" or (errorHandler.buildDepError "process"))
            (hsPkgs."shelly" or (errorHandler.buildDepError "shelly"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            ];
          buildable = if flags.build-examples then true else false;
          };
        };
      tests = {
        "shelly-testsuite" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."async" or (errorHandler.buildDepError "async"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."process" or (errorHandler.buildDepError "process"))
            (hsPkgs."unix-compat" or (errorHandler.buildDepError "unix-compat"))
            (hsPkgs."unix" or (errorHandler.buildDepError "unix"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."hspec-contrib" or (errorHandler.buildDepError "hspec-contrib"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."transformers-base" or (errorHandler.buildDepError "transformers-base"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."monad-control" or (errorHandler.buildDepError "monad-control"))
            (hsPkgs."lifted-base" or (errorHandler.buildDepError "lifted-base"))
            (hsPkgs."lifted-async" or (errorHandler.buildDepError "lifted-async"))
            (hsPkgs."enclosed-exceptions" or (errorHandler.buildDepError "enclosed-exceptions"))
            (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
            ] ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).lt "8.0") (hsPkgs."fail" or (errorHandler.buildDepError "fail"));
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/shelly-1.9.0.tar.gz";
      sha256 = "5eb5fd4fc105e218cef6cfa10971d299ad660324e6a6006b8cccc31edf39aace";
      });
    }