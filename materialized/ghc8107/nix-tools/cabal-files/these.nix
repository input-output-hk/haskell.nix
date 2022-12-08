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
    flags = { assoc = true; };
    package = {
      specVersion = "1.10";
      identifier = { name = "these"; version = "1.1.1.1"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Oleg Grenrus <oleg.grenrus@iki.fi>";
      author = "C. McCann, Oleg Grenrus";
      homepage = "https://github.com/haskellari/these";
      url = "";
      synopsis = "An either-or-both data type.";
      description = "This package provides a data type @These a b@ which can hold a value of either\ntype or values of each type. This is usually thought of as an \"inclusive or\"\ntype (contrasting @Either a b@ as \"exclusive or\") or as an \"outer join\" type\n(contrasting @(a, b)@ as \"inner join\").\n\n@\ndata These a b = This a | That b | These a b\n@\n\nSince version 1, this package was split into parts:\n\n* <https://hackage.haskell.org/package/semialign semialign> For @Align@ and @Zip@ type-classes.\n\n* <https://hackage.haskell.org/package/semialign-indexed semialign-indexed> For @SemialignWithIndex@ class, providing @ialignWith@ and @izipWith@.\n\n* <https://hackage.haskell.org/package/these-lens these-lens> For lens combinators.\n\n* <http://hackage.haskell.org/package/monad-chronicle monad-chronicle> For transformers variant of @These@.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = ((([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          ] ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).lt "7.5") (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))) ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8.2")) (hsPkgs."bifunctors" or (errorHandler.buildDepError "bifunctors"))) ++ (pkgs.lib).optionals (!(compiler.isGhc && (compiler.version).ge "8.0")) ([
          (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."transformers-compat" or (errorHandler.buildDepError "transformers-compat"))
          ] ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).ge "7.10") (hsPkgs."transformers" or (errorHandler.buildDepError "transformers")))) ++ (pkgs.lib).optional (flags.assoc) (hsPkgs."assoc" or (errorHandler.buildDepError "assoc"));
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/these-1.1.1.1.tar.gz";
      sha256 = "d798c9f56e17def441e8f51e54cc11afdb3e76c6a9d1e9ee154e9a78da0bf508";
      });
    }