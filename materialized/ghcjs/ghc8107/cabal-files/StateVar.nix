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
      identifier = { name = "StateVar"; version = "1.2.1"; };
      license = "BSD-3-Clause";
      copyright = "Copyright (C) 2014-2015 Edward A. Kmett, 2009-2018 Sven Panne";
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
      url = "http://hackage.haskell.org/package/StateVar-1.2.1.tar.gz";
      sha256 = "ee261552912b60d8b937f0253615e310e6cc25f9c407001b3bcc2e3d55000f8b";
      });
    }