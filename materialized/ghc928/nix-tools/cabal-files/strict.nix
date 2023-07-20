{ system
  , compiler
  , flags
  , pkgs
  , hsPkgs
  , pkgconfPkgs
  , errorHandler
  , config
  , ... }:
  ({
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
    }) // {
    package-description-override = "Name:           strict\nVersion:        0.4.0.1\nx-revision:     4\nSynopsis:       Strict data types and String IO.\nCategory:       Data, System\nDescription:\n    This package provides strict versions of some standard Haskell data\n    types (pairs, Maybe and Either). It also contains strict IO operations.\n    .\n    It is common knowledge that lazy datastructures can lead to space-leaks.\n    This problem is particularly prominent, when using lazy datastructures to\n    store the state of a long-running application in memory. One common\n    solution to this problem is to use @seq@ and its variants in every piece of\n    code that updates your state. However a much easier solution is to use\n    fully strict types to store such state values. By \\\"fully strict types\\\" we\n    mean types for whose values it holds that, if they are in weak-head normal\n    form, then they are also in normal form. Intuitively, this means that\n    values of fully strict types cannot contain unevaluated thunks.\n    .\n    To define a fully strict datatype, one typically uses the following recipe.\n    .\n    1. Make all fields of every constructor strict; i.e., add a bang to\n    all fields.\n    .\n    2. Use only strict types for the fields of the constructors.\n    .\n    The second requirement is problematic as it rules out the use of\n    the standard Haskell 'Maybe', 'Either', and pair types. This library\n    solves this problem by providing strict variants of these types and their\n    corresponding standard support functions and type-class instances.\n    .\n    Note that this library does currently not provide fully strict lists.\n    They can be added if they are really required. However, in many cases one\n    probably wants to use unboxed or strict boxed vectors from the 'vector'\n    library (<http://hackage.haskell.org/package/vector>) instead of strict\n    lists.  Moreover, instead of @String@s one probably wants to use strict\n    @Text@ values from the @text@ library\n    (<http://hackage.haskell.org/package/text>).\n    .\n    This library comes with batteries included; i.e., mirror functions and\n    instances of the lazy versions in @base@. It also includes instances for\n    type-classes from the @deepseq@, @binary@, and @hashable@ packages.\nLicense:        BSD3\nLicense-File:   LICENSE\nAuthor:         Roman Leshchinskiy <rl@cse.unsw.edu.au>\n                Simon Meier <iridcode@gmail.com>\nMaintainer:     Don Stewart <dons@galois.com>,\n                Bas van Dijk <v.dijk.bas@gmail.com>,\n                Oleg Grenrus <oleg.grenrus@iki.fi>,\n                Simon Meier <iridcode@gmail.com>,\n                Ximin Luo <infinity0@pwned.gg>\nCopyright:      (c) 2006-2008 by Roman Leshchinskiy\n                (c) 2013-2014 by Simon Meier\nHomepage:       https://github.com/haskell-strict/strict\nCabal-Version: >= 1.10\nBuild-type:     Simple\nextra-source-files: CHANGELOG.md\ntested-with:\n  GHC ==7.4.2\n   || ==7.6.3\n   || ==7.8.4\n   || ==7.10.3\n   || ==8.0.2\n   || ==8.2.2\n   || ==8.4.4\n   || ==8.6.5\n   || ==8.8.3\n   || ==8.10.4\n   || ==9.0.1\n   || ==9.2.1\n\nflag assoc\n  description: Build with assoc dependency\n  manual:      True\n  default:     True\n\nlibrary\n  default-language: Haskell2010\n  hs-source-dirs:   src\n  ghc-options:      -Wall\n\n  build-depends:\n      base         >= 4.5.0.0 && < 5\n    , binary       >= 0.5.1.0 && < 0.9\n    , bytestring   >= 0.9.2.1 && < 0.12\n    , deepseq      >= 1.3.0.0 && < 1.5\n    , hashable     >= 1.2.7.0 && < 1.5\n    , text         >= 1.2.3.0 && < 1.3 || >=2.0 && <2.1\n    , these        >= 1.1.1.1 && < 1.2\n    , transformers >= 0.3.0.0 && < 0.7\n    , ghc-prim\n\n  if !impl(ghc >= 8.0)\n    build-depends:\n        semigroups           >= 0.18.5  && < 0.21\n      , transformers-compat  >= 0.6.5   && < 0.8\n\n    -- Ensure Data.Functor.Classes is always available\n    if impl(ghc >= 7.10)\n      build-depends: transformers >= 0.4.2.0\n\n  if !impl(ghc >= 8.2)\n    build-depends:\n      bifunctors >= 5.5.2 && < 5.6\n\n  if flag(assoc)\n    build-depends: assoc >= 1.0.1 && < 1.1\n\n  exposed-modules:\n    Data.Strict\n    Data.Strict.Classes\n    Data.Strict.These\n    Data.Strict.Tuple\n    Data.Strict.Maybe\n    Data.Strict.Either\n    System.IO.Strict\n";
    }