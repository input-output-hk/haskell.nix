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
    flags = { network-uri = true; developer = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "webdriver"; version = "0.9.0.1"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "kallisti.dev@gmail.com";
      author = "Adam Curtis";
      homepage = "https://github.com/kallisti-dev/hs-webdriver";
      url = "";
      synopsis = "a Haskell client for the Selenium WebDriver protocol";
      description = "A Selenium WebDriver client for Haskell.\nYou can use it to automate browser sessions\nfor testing, system administration, etc.\n\nFor more information about Selenium itself, see\n<http://seleniumhq.org/>\n\nTo find out what's been changed in this version and others,\nsee the change log at\n<https://github.com/kallisti-dev/hs-webdriver/blob/master/CHANGELOG.md>";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."http-client" or (errorHandler.buildDepError "http-client"))
          (hsPkgs."http-types" or (errorHandler.buildDepError "http-types"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."attoparsec" or (errorHandler.buildDepError "attoparsec"))
          (hsPkgs."base64-bytestring" or (errorHandler.buildDepError "base64-bytestring"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."monad-control" or (errorHandler.buildDepError "monad-control"))
          (hsPkgs."transformers-base" or (errorHandler.buildDepError "transformers-base"))
          (hsPkgs."lifted-base" or (errorHandler.buildDepError "lifted-base"))
          (hsPkgs."zip-archive" or (errorHandler.buildDepError "zip-archive"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."directory-tree" or (errorHandler.buildDepError "directory-tree"))
          (hsPkgs."temporary" or (errorHandler.buildDepError "temporary"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
          (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
          (hsPkgs."data-default-class" or (errorHandler.buildDepError "data-default-class"))
          (hsPkgs."call-stack" or (errorHandler.buildDepError "call-stack"))
          ] ++ [
          (hsPkgs."network-uri" or (errorHandler.buildDepError "network-uri"))
          (hsPkgs."network" or (errorHandler.buildDepError "network"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/webdriver-0.9.0.1.tar.gz";
      sha256 = "135950889784b9d323c70ebf7ecd75b8df194489a303d85995b1fccc7549dff0";
      });
    }