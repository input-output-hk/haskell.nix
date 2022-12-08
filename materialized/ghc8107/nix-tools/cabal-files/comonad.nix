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
    flags = {
      containers = true;
      distributive = true;
      indexed-traversable = true;
      };
    package = {
      specVersion = "1.10";
      identifier = { name = "comonad"; version = "5.0.8"; };
      license = "BSD-3-Clause";
      copyright = "Copyright (C) 2008-2014 Edward A. Kmett,\nCopyright (C) 2004-2008 Dave Menendez";
      maintainer = "Edward A. Kmett <ekmett@gmail.com>";
      author = "Edward A. Kmett";
      homepage = "http://github.com/ekmett/comonad/";
      url = "";
      synopsis = "Comonads";
      description = "Comonads.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = ((([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."tagged" or (errorHandler.buildDepError "tagged"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."transformers-compat" or (errorHandler.buildDepError "transformers-compat"))
          ] ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8.0")) (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))) ++ (pkgs.lib).optional (flags.containers) (hsPkgs."containers" or (errorHandler.buildDepError "containers"))) ++ (pkgs.lib).optional (flags.distributive) (hsPkgs."distributive" or (errorHandler.buildDepError "distributive"))) ++ (pkgs.lib).optional (flags.indexed-traversable) (hsPkgs."indexed-traversable" or (errorHandler.buildDepError "indexed-traversable"));
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/comonad-5.0.8.tar.gz";
      sha256 = "ef6cdf2cc292cc43ee6aa96c581b235fdea8ab44a0bffb24dc79ae2b2ef33d13";
      });
    }