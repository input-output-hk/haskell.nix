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
      identifier = { name = "regex-posix"; version = "0.96.0.0"; };
      license = "BSD-3-Clause";
      copyright = "Copyright (c) 2007-2010, Christopher Kuklewicz";
      maintainer = "hvr@gnu.org";
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
          ] ++ (if flags._regex-posix-clib
          then [
            (hsPkgs."regex-posix-clib" or (errorHandler.buildDepError "regex-posix-clib"))
            ]
          else (pkgs.lib).optional (system.isWindows) (hsPkgs."base" or (errorHandler.buildDepError "base")))) ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8")) (hsPkgs."fail" or (errorHandler.buildDepError "fail"));
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/regex-posix-0.96.0.0.tar.gz";
      sha256 = "251300f1a6bb2e91abb8bf513a21981f8fab79c98a65acea2bb6d6a524414521";
      });
    }