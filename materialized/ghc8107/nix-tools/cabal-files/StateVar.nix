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
      identifier = { name = "StateVar"; version = "1.2.2"; };
      license = "BSD-3-Clause";
      copyright = "Copyright (C) 2014-2015 Edward A. Kmett, 2009-2021 Sven Panne";
      maintainer = "Sven Panne <svenpanne@gmail.com>";
      author = "Sven Panne and Edward Kmett";
      homepage = "https://github.com/haskell-opengl/StateVar";
      url = "";
      synopsis = "State variables";
      description = "This package contains state variables, which are references in the IO monad,\nlike IORefs or parts of the OpenGL state.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/StateVar-1.2.2.tar.gz";
      sha256 = "5e4b39da395656a59827b0280508aafdc70335798b50e5d6fd52596026251825";
      });
    }