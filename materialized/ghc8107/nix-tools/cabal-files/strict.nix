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
      identifier = { name = "strict"; version = "0.4.0.1"; };
      license = "BSD-3-Clause";
      copyright = "(c) 2006-2008 by Roman Leshchinskiy\n(c) 2013-2014 by Simon Meier";
      maintainer = "Don Stewart <dons@galois.com>,\nBas van Dijk <v.dijk.bas@gmail.com>,\nOleg Grenrus <oleg.grenrus@iki.fi>,\nSimon Meier <iridcode@gmail.com>,\nXimin Luo <infinity0@pwned.gg>";
      author = "Roman Leshchinskiy <rl@cse.unsw.edu.au>\nSimon Meier <iridcode@gmail.com>";
      homepage = "https://github.com/haskell-strict/strict";
      url = "";
      synopsis = "Strict data types and String IO.";
      description = "This package provides strict versions of some standard Haskell data\ntypes (pairs, Maybe and Either). It also contains strict IO operations.\n\nIt is common knowledge that lazy datastructures can lead to space-leaks.\nThis problem is particularly prominent, when using lazy datastructures to\nstore the state of a long-running application in memory. One common\nsolution to this problem is to use @seq@ and its variants in every piece of\ncode that updates your state. However a much easier solution is to use\nfully strict types to store such state values. By \\\"fully strict types\\\" we\nmean types for whose values it holds that, if they are in weak-head normal\nform, then they are also in normal form. Intuitively, this means that\nvalues of fully strict types cannot contain unevaluated thunks.\n\nTo define a fully strict datatype, one typically uses the following recipe.\n\n1. Make all fields of every constructor strict; i.e., add a bang to\nall fields.\n\n2. Use only strict types for the fields of the constructors.\n\nThe second requirement is problematic as it rules out the use of\nthe standard Haskell 'Maybe', 'Either', and pair types. This library\nsolves this problem by providing strict variants of these types and their\ncorresponding standard support functions and type-class instances.\n\nNote that this library does currently not provide fully strict lists.\nThey can be added if they are really required. However, in many cases one\nprobably wants to use unboxed or strict boxed vectors from the 'vector'\nlibrary (<http://hackage.haskell.org/package/vector>) instead of strict\nlists.  Moreover, instead of @String@s one probably wants to use strict\n@Text@ values from the @text@ library\n(<http://hackage.haskell.org/package/text>).\n\nThis library comes with batteries included; i.e., mirror functions and\ninstances of the lazy versions in @base@. It also includes instances for\ntype-classes from the @deepseq@, @binary@, and @hashable@ packages.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = (([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."these" or (errorHandler.buildDepError "these"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
          ] ++ (pkgs.lib).optionals (!(compiler.isGhc && (compiler.version).ge "8.0")) ([
          (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
          (hsPkgs."transformers-compat" or (errorHandler.buildDepError "transformers-compat"))
          ] ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).ge "7.10") (hsPkgs."transformers" or (errorHandler.buildDepError "transformers")))) ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8.2")) (hsPkgs."bifunctors" or (errorHandler.buildDepError "bifunctors"))) ++ (pkgs.lib).optional (flags.assoc) (hsPkgs."assoc" or (errorHandler.buildDepError "assoc"));
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/strict-0.4.0.1.tar.gz";
      sha256 = "dff6abc08ad637e51891bb8b475778c40926c51219eda60fd64f0d9680226241";
      });
    }