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
    flags = { _regex-posix-clib = false; };
    package = {
      specVersion = "1.12";
      identifier = { name = "regex-posix"; version = "0.96.0.1"; };
      license = "BSD-3-Clause";
      copyright = "Copyright (c) 2007-2010, Christopher Kuklewicz";
      maintainer = "Andreas Abel";
      author = "Christopher Kuklewicz";
      homepage = "";
      url = "";
      synopsis = "POSIX Backend for \"Text.Regex\" (regex-base)";
      description = "The POSIX regex backend for <//hackage.haskell.org/package/regex-base regex-base>.\n\nThe main appeal of this backend is that it's very lightweight due to its reliance on the ubiquitous <https://pubs.opengroup.org/onlinepubs/9699919799/basedefs/regex.h.html POSIX.2 regex> facility that is provided by the standard C library on most POSIX platforms.\n\nSee also <https://wiki.haskell.org/Regular_expressions> for more information.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = ([
          (hsPkgs."regex-base" or (errorHandler.buildDepError "regex-base"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          ] ++ (pkgs.lib).optional (flags._regex-posix-clib || system.isWindows) (hsPkgs."regex-posix-clib" or (errorHandler.buildDepError "regex-posix-clib"))) ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).lt "8") (hsPkgs."fail" or (errorHandler.buildDepError "fail"));
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/regex-posix-0.96.0.1.tar.gz";
      sha256 = "c7827c391919227711e1cff0a762b1678fd8739f9c902fc183041ff34f59259c";
      });
    }