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
      identifier = { name = "regex-base"; version = "0.94.0.2"; };
      license = "BSD-3-Clause";
      copyright = "Copyright (c) 2006, Christopher Kuklewicz";
      maintainer = "Andreas Abel";
      author = "Christopher Kuklewicz";
      homepage = "https://wiki.haskell.org/Regular_expressions";
      url = "";
      synopsis = "Common \"Text.Regex.*\" API for Regex matching";
      description = "This package does not provide the ability to do regular expression matching.\nInstead, it provides the type classes that constitute the abstract API\nthat is implemented by @regex-*@ backends such as:\n\n* <https://hackage.haskell.org/package/regex-posix regex-posix>\n\n* <https://hackage.haskell.org/package/regex-parsec regex-parsec>\n\n* <https://hackage.haskell.org/package/regex-dfa regex-dfa>\n\n* <https://hackage.haskell.org/package/regex-tdfa regex-tdfa>\n\n* <https://hackage.haskell.org/package/regex-pcre regex-pcre>\n\nSee also <https://wiki.haskell.org/Regular_expressions> for more information.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = ([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          ] ++ (pkgs.lib).optionals (compiler.isGhc && (compiler.version).ge "7.4") [
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          ]) ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8")) (hsPkgs."fail" or (errorHandler.buildDepError "fail"));
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/regex-base-0.94.0.2.tar.gz";
      sha256 = "7b99408f580f5bb67a1c413e0bc735886608251331ad36322020f2169aea2ef1";
      });
    }