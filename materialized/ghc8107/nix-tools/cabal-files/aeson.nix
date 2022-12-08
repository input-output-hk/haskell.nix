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
    flags = { cffi = false; ordered-keymap = true; };
    package = {
      specVersion = "1.10";
      identifier = { name = "aeson"; version = "2.0.3.0"; };
      license = "BSD-3-Clause";
      copyright = "(c) 2011-2016 Bryan O'Sullivan\n(c) 2011 MailRank, Inc.";
      maintainer = "Adam Bergmark <adam@bergmark.nl>";
      author = "Bryan O'Sullivan <bos@serpentine.com>";
      homepage = "https://github.com/haskell/aeson";
      url = "";
      synopsis = "Fast JSON parsing and encoding";
      description = "A JSON parsing and encoding library optimized for ease of use\nand high performance.\n\nTo get started, see the documentation for the @Data.Aeson@ module\nbelow.\n\n(A note on naming: in Greek mythology, Aeson was the father of Jason.)";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = ([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."base-compat-batteries" or (errorHandler.buildDepError "base-compat-batteries"))
          (hsPkgs."time-compat" or (errorHandler.buildDepError "time-compat"))
          (hsPkgs."attoparsec" or (errorHandler.buildDepError "attoparsec"))
          (hsPkgs."data-fix" or (errorHandler.buildDepError "data-fix"))
          (hsPkgs."dlist" or (errorHandler.buildDepError "dlist"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."indexed-traversable" or (errorHandler.buildDepError "indexed-traversable"))
          (hsPkgs."OneTuple" or (errorHandler.buildDepError "OneTuple"))
          (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
          (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
          (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
          (hsPkgs."semialign" or (errorHandler.buildDepError "semialign"))
          (hsPkgs."strict" or (errorHandler.buildDepError "strict"))
          (hsPkgs."tagged" or (errorHandler.buildDepError "tagged"))
          (hsPkgs."text-short" or (errorHandler.buildDepError "text-short"))
          (hsPkgs."th-abstraction" or (errorHandler.buildDepError "th-abstraction"))
          (hsPkgs."these" or (errorHandler.buildDepError "these"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          (hsPkgs."uuid-types" or (errorHandler.buildDepError "uuid-types"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          (hsPkgs."witherable" or (errorHandler.buildDepError "witherable"))
          ] ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8.6")) (hsPkgs."contravariant" or (errorHandler.buildDepError "contravariant"))) ++ (pkgs.lib).optional (!(compiler.isGhcjs && true || !flags.cffi)) (hsPkgs."text" or (errorHandler.buildDepError "text"));
        buildable = true;
        };
      tests = {
        "aeson-tests" = {
          depends = [
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."attoparsec" or (errorHandler.buildDepError "attoparsec"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base-compat" or (errorHandler.buildDepError "base-compat"))
            (hsPkgs."base-orphans" or (errorHandler.buildDepError "base-orphans"))
            (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."data-fix" or (errorHandler.buildDepError "data-fix"))
            (hsPkgs."Diff" or (errorHandler.buildDepError "Diff"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."dlist" or (errorHandler.buildDepError "dlist"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."generic-deriving" or (errorHandler.buildDepError "generic-deriving"))
            (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
            (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
            (hsPkgs."indexed-traversable" or (errorHandler.buildDepError "indexed-traversable"))
            (hsPkgs."integer-logarithms" or (errorHandler.buildDepError "integer-logarithms"))
            (hsPkgs."OneTuple" or (errorHandler.buildDepError "OneTuple"))
            (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."quickcheck-instances" or (errorHandler.buildDepError "quickcheck-instances"))
            (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
            (hsPkgs."strict" or (errorHandler.buildDepError "strict"))
            (hsPkgs."tagged" or (errorHandler.buildDepError "tagged"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-golden" or (errorHandler.buildDepError "tasty-golden"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."text-short" or (errorHandler.buildDepError "text-short"))
            (hsPkgs."these" or (errorHandler.buildDepError "these"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."time-compat" or (errorHandler.buildDepError "time-compat"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            (hsPkgs."uuid-types" or (errorHandler.buildDepError "uuid-types"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/aeson-2.0.3.0.tar.gz";
      sha256 = "19760a2b84764fa3cee65c63df0c80d45497e2f0c3b95d4e6dc2083b8604b325";
      });
    }