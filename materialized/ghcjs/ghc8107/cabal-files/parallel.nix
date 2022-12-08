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
      identifier = { name = "parallel"; version = "3.2.2.0"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "libraries@haskell.org";
      author = "";
      homepage = "";
      url = "";
      synopsis = "Parallel programming library";
      description = "This package provides a library for parallel programming.\n\nFor documentation start from the \"Control.Parallel.Strategies\"\nmodule below.\n\nFor more tutorial documentation, see the book <http://simonmar.github.io/pages/pcph.html Parallel and Concurrent Programming in Haskell>.\n\nTo understand the principles behind the library, see\n<http://simonmar.github.io/bib/papers/strategies.pdf Seq no more: Better Strategies for Parallel Haskell>.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          ] ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).ge "7.2.1") (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"));
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/parallel-3.2.2.0.tar.gz";
      sha256 = "170453a71a2a8b31cca63125533f7771d7debeb639700bdabdd779c34d8a6ef6";
      });
    }