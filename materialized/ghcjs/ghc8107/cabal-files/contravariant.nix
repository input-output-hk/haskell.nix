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
    flags = { tagged = true; semigroups = true; statevar = true; };
    package = {
      specVersion = "1.10";
      identifier = { name = "contravariant"; version = "1.5.3"; };
      license = "BSD-3-Clause";
      copyright = "Copyright (C) 2007-2015 Edward A. Kmett";
      maintainer = "Edward A. Kmett <ekmett@gmail.com>";
      author = "Edward A. Kmett";
      homepage = "http://github.com/ekmett/contravariant/";
      url = "";
      synopsis = "Contravariant functors";
      description = "Contravariant functors.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = ((((([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          ] ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).gt "7.10")) (hsPkgs."transformers-compat" or (errorHandler.buildDepError "transformers-compat"))) ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "7.9")) (hsPkgs."void" or (errorHandler.buildDepError "void"))) ++ (pkgs.lib).optional (flags.tagged && !(compiler.isGhc && (compiler.version).ge "7.7")) (hsPkgs."tagged" or (errorHandler.buildDepError "tagged"))) ++ (pkgs.lib).optional (flags.semigroups && !(compiler.isGhc && (compiler.version).ge "7.11")) (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))) ++ (pkgs.lib).optional (flags.statevar) (hsPkgs."StateVar" or (errorHandler.buildDepError "StateVar"))) ++ (pkgs.lib).optional (compiler.isGhc && ((compiler.version).ge "7.2" && (compiler.version).lt "7.6")) (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"));
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/contravariant-1.5.3.tar.gz";
      sha256 = "44536f0e331fde471271937323dc90409e95d47f57e42657fdaf242a0fd65dc1";
      });
    }