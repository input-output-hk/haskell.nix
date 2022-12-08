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
      identifier = { name = "OneTuple"; version = "0.3.1"; };
      license = "BSD-3-Clause";
      copyright = "(c) John Dorsey 2008";
      maintainer = "Oleg Grenrus <oleg.grenrus@iki.fi>, John Dorsey <haskell@colquitt.org>";
      author = "John Dorsey <haskell@colquitt.org>";
      homepage = "";
      url = "";
      synopsis = "Singleton Tuple";
      description = "This package is a compatibility package for a singleton data type\n\n> data Solo a = Solo a\n\nNote: it's not a @newtype@\n\n@Solo@ is available in @base-4.16@ (GHC-9.2).";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = ((([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          ] ++ (if compiler.isGhc && (compiler.version).ge "9.0"
          then [
            (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
            ]
          else [
            (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
            ])) ++ (pkgs.lib).optionals (!(compiler.isGhc && (compiler.version).ge "8.0")) ([
          (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          ] ++ (if compiler.isGhc && (compiler.version).ge "7.10"
          then [
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            ]
          else [
            (hsPkgs."transformers-compat" or (errorHandler.buildDepError "transformers-compat"))
            ]))) ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "9.2")) (hsPkgs."base-orphans" or (errorHandler.buildDepError "base-orphans"))) ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "7.6")) (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"));
        buildable = true;
        };
      tests = {
        "instances" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
            (hsPkgs."OneTuple" or (errorHandler.buildDepError "OneTuple"))
            ] ++ (pkgs.lib).optionals (!(compiler.isGhc && (compiler.version).ge "8.0")) [
            (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."transformers-compat" or (errorHandler.buildDepError "transformers-compat"))
            ];
          buildable = true;
          };
        "th" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."OneTuple" or (errorHandler.buildDepError "OneTuple"))
            (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/OneTuple-0.3.1.tar.gz";
      sha256 = "98853682d52fb4cc37a45cd186fbd77cf2565d3df5171acc4cf026427e103eef";
      });
    }