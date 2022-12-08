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
    flags = { old-time = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "unix-compat"; version = "0.5.4"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Jacob Stanley <jacob@stanley.io>";
      author = "Björn Bringert, Duncan Coutts, Jacob Stanley, Bryan O'Sullivan";
      homepage = "http://github.com/jacobstanley/unix-compat";
      url = "";
      synopsis = "Portable POSIX-compatibility layer.";
      description = "This package provides portable implementations of parts\nof the unix package. This package re-exports the unix\npackage when available. When it isn't available,\nportable implementations are used.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          ] ++ (if system.isWindows
          then [
            (hsPkgs."Win32" or (errorHandler.buildDepError "Win32"))
            ] ++ (if flags.old-time
            then [
              (hsPkgs."old-time" or (errorHandler.buildDepError "old-time"))
              ] ++ [
              (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
              ]
            else [
              (hsPkgs."time" or (errorHandler.buildDepError "time"))
              (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
              ])
          else [ (hsPkgs."unix" or (errorHandler.buildDepError "unix")) ]);
        libs = (pkgs.lib).optional (system.isWindows) (pkgs."msvcrt" or (errorHandler.sysDepError "msvcrt"));
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/unix-compat-0.5.4.tar.gz";
      sha256 = "8224579d6e9acea7ecbd7af21f191758a11c77a2c5b2fc61f1079ac004a4a4b1";
      });
    }