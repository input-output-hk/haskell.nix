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
      hashable = true;
      binary = true;
      bytestring = true;
      bytestring-builder = false;
      containers = true;
      deepseq = true;
      tagged = true;
      template-haskell = true;
      text = true;
      transformers = true;
      unordered-containers = true;
      };
    package = {
      specVersion = "1.10";
      identifier = { name = "semigroups"; version = "0.19.1"; };
      license = "BSD-3-Clause";
      copyright = "Copyright (C) 2011-2015 Edward A. Kmett";
      maintainer = "Edward A. Kmett <ekmett@gmail.com>";
      author = "Edward A. Kmett";
      homepage = "http://github.com/ekmett/semigroups/";
      url = "";
      synopsis = "Anything that associates";
      description = "In mathematics, a semigroup is an algebraic structure consisting of a set together with an associative binary operation. A semigroup generalizes a monoid in that there might not exist an identity element. It also (originally) generalized a group (a monoid with all inverses) to a type where every element did not have to have an inverse, thus the name semigroup.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          ] ++ (pkgs.lib).optionals (compiler.isGhc && (compiler.version).lt "7.11.20151002") ((((((((((((pkgs.lib).optional (compiler.isGhc && (compiler.version).lt "7.10") (hsPkgs."nats" or (errorHandler.buildDepError "nats")) ++ (pkgs.lib).optional (compiler.isGhc && ((compiler.version).ge "7.2" && (compiler.version).lt "7.5")) (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))) ++ (pkgs.lib).optional (flags.binary) (hsPkgs."binary" or (errorHandler.buildDepError "binary"))) ++ (pkgs.lib).optionals (flags.bytestring) (if flags.bytestring-builder
          then [
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."bytestring-builder" or (errorHandler.buildDepError "bytestring-builder"))
            ]
          else [
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            ])) ++ (pkgs.lib).optional (flags.containers) (hsPkgs."containers" or (errorHandler.buildDepError "containers"))) ++ (pkgs.lib).optional (flags.deepseq) (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))) ++ (pkgs.lib).optional (flags.tagged) (hsPkgs."tagged" or (errorHandler.buildDepError "tagged"))) ++ (pkgs.lib).optional (flags.text) (hsPkgs."text" or (errorHandler.buildDepError "text"))) ++ (pkgs.lib).optional (flags.hashable) (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))) ++ (pkgs.lib).optional (flags.hashable && flags.unordered-containers) (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))) ++ (pkgs.lib).optionals (flags.transformers) [
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."transformers-compat" or (errorHandler.buildDepError "transformers-compat"))
          ]) ++ (pkgs.lib).optional (flags.template-haskell) (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell")));
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/semigroups-0.19.1.tar.gz";
      sha256 = "79e761e64b862564a3470d5d356cb6b060b14452d675859aed3b2d1e14646648";
      });
    }