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
      identifier = { name = "old-time"; version = "1.1.0.3"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "libraries@haskell.org";
      author = "";
      homepage = "";
      url = "";
      synopsis = "Time library";
      description = "This package provides the old time library.\n\nFor new projects, the newer\n<http://hackage.haskell.org/package/time time library>\nis recommended.";
      buildType = "Configure";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."old-locale" or (errorHandler.buildDepError "old-locale"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/old-time-1.1.0.3.tar.gz";
      sha256 = "1ccb158b0f7851715d36b757c523b026ca1541e2030d02239802ba39b4112bc1";
      });
    }