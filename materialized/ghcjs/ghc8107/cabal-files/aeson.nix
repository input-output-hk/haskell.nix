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
      developer = false;
      fast = false;
      bytestring-builder = false;
      cffi = false;
      };
    package = {
      specVersion = "1.10";
      identifier = { name = "aeson"; version = "1.4.7.1"; };
      license = "BSD-3-Clause";
      copyright = "(c) 2011-2016 Bryan O'Sullivan\n(c) 2011 MailRank, Inc.";
      maintainer = "Adam Bergmark <adam@bergmark.nl>";
      author = "Bryan O'Sullivan <bos@serpentine.com>";
      homepage = "https://github.com/bos/aeson";
      url = "";
      synopsis = "Fast JSON parsing and encoding";
      description = "A JSON parsing and encoding library optimized for ease of use\nand high performance.\n\nTo get started, see the documentation for the @Data.Aeson@ module\nbelow.\n\n(A note on naming: in Greek mythology, Aeson was the father of Jason.)";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = ((([
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
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          (hsPkgs."tagged" or (errorHandler.buildDepError "tagged"))
          (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
          (hsPkgs."attoparsec" or (errorHandler.buildDepError "attoparsec"))
          (hsPkgs."dlist" or (errorHandler.buildDepError "dlist"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
          (hsPkgs."th-abstraction" or (errorHandler.buildDepError "th-abstraction"))
          (hsPkgs."uuid-types" or (errorHandler.buildDepError "uuid-types"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          ] ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).ge "8.0") (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))) ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8.6")) (hsPkgs."contravariant" or (errorHandler.buildDepError "contravariant"))) ++ (pkgs.lib).optionals (!(compiler.isGhc && (compiler.version).ge "8.0")) [
          (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."transformers-compat" or (errorHandler.buildDepError "transformers-compat"))
          (hsPkgs."fail" or (errorHandler.buildDepError "fail"))
          ]) ++ (pkgs.lib).optionals (!(compiler.isGhc && (compiler.version).ge "7.10")) [
          (hsPkgs."nats" or (errorHandler.buildDepError "nats"))
          (hsPkgs."void" or (errorHandler.buildDepError "void"))
          ];
        buildable = true;
        };
      tests = {
        "tests" = {
          depends = ((([
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."integer-logarithms" or (errorHandler.buildDepError "integer-logarithms"))
            (hsPkgs."attoparsec" or (errorHandler.buildDepError "attoparsec"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base-compat" or (errorHandler.buildDepError "base-compat"))
            (hsPkgs."base-orphans" or (errorHandler.buildDepError "base-orphans"))
            (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."dlist" or (errorHandler.buildDepError "dlist"))
            (hsPkgs."Diff" or (errorHandler.buildDepError "Diff"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."generic-deriving" or (errorHandler.buildDepError "generic-deriving"))
            (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
            (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
            (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
            (hsPkgs."tagged" or (errorHandler.buildDepError "tagged"))
            (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-golden" or (errorHandler.buildDepError "tasty-golden"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."time-compat" or (errorHandler.buildDepError "time-compat"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            (hsPkgs."uuid-types" or (errorHandler.buildDepError "uuid-types"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."quickcheck-instances" or (errorHandler.buildDepError "quickcheck-instances"))
            ] ++ (if flags.bytestring-builder
            then [
              (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
              (hsPkgs."bytestring-builder" or (errorHandler.buildDepError "bytestring-builder"))
              ]
            else [
              (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
              ])) ++ (pkgs.lib).optionals (!(compiler.isGhc && (compiler.version).ge "8.0")) [
            (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."transformers-compat" or (errorHandler.buildDepError "transformers-compat"))
            ]) ++ (pkgs.lib).optionals (!(compiler.isGhc && (compiler.version).ge "7.10")) [
            (hsPkgs."nats" or (errorHandler.buildDepError "nats"))
            (hsPkgs."void" or (errorHandler.buildDepError "void"))
            ]) ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).ge "7.8") (hsPkgs."hashable-time" or (errorHandler.buildDepError "hashable-time"));
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/aeson-1.4.7.1.tar.gz";
      sha256 = "07e746655fd9bec81c59927c5617877ff4fcd81d0df45c5fb8ef154fb8f40294";
      });
    }