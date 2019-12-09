let
  buildDepError = pkg:
    builtins.throw ''
      The Haskell package set does not contain the package: ${pkg} (build dependency).
      
      If you are using Stackage, make sure that you are using a snapshot that contains the package. Otherwise you may need to update the Hackage snapshot you are using, usually by updating haskell.nix.
      '';
  sysDepError = pkg:
    builtins.throw ''
      The Nixpkgs package set does not contain the package: ${pkg} (system dependency).
      
      You may need to augment the system package mapping in haskell.nix so that it can be found.
      '';
  pkgConfDepError = pkg:
    builtins.throw ''
      The pkg-conf packages does not contain the package: ${pkg} (pkg-conf dependency).
      
      You may need to augment the pkg-conf package mapping in haskell.nix so that it can be found.
      '';
  exeDepError = pkg:
    builtins.throw ''
      The local executable components do not include the component: ${pkg} (executable dependency).
      '';
  legacyExeDepError = pkg:
    builtins.throw ''
      The Haskell package set does not contain the package: ${pkg} (executable dependency).
      
      If you are using Stackage, make sure that you are using a snapshot that contains the package. Otherwise you may need to update the Hackage snapshot you are using, usually by updating haskell.nix.
      '';
  buildToolDepError = pkg:
    builtins.throw ''
      Neither the Haskell package set or the Nixpkgs package set contain the package: ${pkg} (build tool dependency).
      
      If this is a system dependency:
      You may need to augment the system package mapping in haskell.nix so that it can be found.
      
      If this is a Haskell dependency:
      If you are using Stackage, make sure that you are using a snapshot that contains the package. Otherwise you may need to update the Hackage snapshot you are using, usually by updating haskell.nix.
      '';
in { system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = { small_base = true; };
    package = {
      specVersion = "1.8";
      identifier = { name = "happy"; version = "1.19.11"; };
      license = "BSD-2-Clause";
      copyright = "(c) Andy Gill, Simon Marlow";
      maintainer = "Simon Marlow <marlowsd@gmail.com>";
      author = "Andy Gill and Simon Marlow";
      homepage = "https://www.haskell.org/happy/";
      url = "";
      synopsis = "Happy is a parser generator for Haskell";
      description = "Happy is a parser generator for Haskell.  Given a grammar\nspecification in BNF, Happy generates Haskell code to parse the\ngrammar.  Happy works in a similar way to the @yacc@ tool for C.";
      buildType = "Custom";
      isLocal = true;
      setup-depends = [
        (hsPkgs.buildPackages.Cabal or (pkgs.buildPackages.Cabal or (buildToolDepError "Cabal")))
        (hsPkgs.buildPackages.base or (pkgs.buildPackages.base or (buildToolDepError "base")))
        (hsPkgs.buildPackages.directory or (pkgs.buildPackages.directory or (buildToolDepError "directory")))
        (hsPkgs.buildPackages.filepath or (pkgs.buildPackages.filepath or (buildToolDepError "filepath")))
        ];
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" ];
      dataDir = "";
      dataFiles = [];
      extraSrcFiles = [
        "ANNOUNCE"
        "CHANGES"
        "Makefile"
        "README.md"
        "TODO"
        "doc/Makefile"
        "doc/aclocal.m4"
        "doc/config.mk.in"
        "doc/configure.ac"
        "doc/docbook-xml.mk"
        "doc/fptools.css"
        "doc/happy.1.in"
        "doc/happy.xml"
        "examples/glr/nlp/Main.lhs"
        "examples/glr/nlp/Makefile"
        "examples/glr/nlp/README"
        "examples/glr/nlp/English.y"
        "examples/glr/nlp/Hugs.lhs"
        "examples/glr/Makefile"
        "examples/glr/Makefile.defs"
        "examples/glr/expr-eval/Main.lhs"
        "examples/glr/expr-eval/Makefile"
        "examples/glr/expr-eval/Expr.y"
        "examples/glr/expr-eval/README"
        "examples/glr/expr-eval/Hugs.lhs"
        "examples/glr/expr-tree/Main.lhs"
        "examples/glr/expr-tree/Makefile"
        "examples/glr/expr-tree/Expr.y"
        "examples/glr/expr-tree/README"
        "examples/glr/expr-tree/Tree.lhs"
        "examples/glr/expr-tree/Hugs.lhs"
        "examples/glr/highly-ambiguous/Main.lhs"
        "examples/glr/highly-ambiguous/Makefile"
        "examples/glr/highly-ambiguous/Expr.y"
        "examples/glr/highly-ambiguous/README"
        "examples/glr/highly-ambiguous/Hugs.lhs"
        "examples/glr/hidden-leftrec/Main.lhs"
        "examples/glr/hidden-leftrec/Makefile"
        "examples/glr/hidden-leftrec/Expr.y"
        "examples/glr/hidden-leftrec/README"
        "examples/glr/hidden-leftrec/Hugs.lhs"
        "examples/glr/expr-monad/Main.lhs"
        "examples/glr/expr-monad/Makefile"
        "examples/glr/expr-monad/Expr.y"
        "examples/glr/expr-monad/README"
        "examples/glr/expr-monad/Hugs.lhs"
        "examples/glr/bio-eg/Main.lhs"
        "examples/glr/bio-eg/Makefile"
        "examples/glr/bio-eg/Bio.y"
        "examples/glr/bio-eg/README"
        "examples/glr/bio-eg/1-1200.dna"
        "examples/glr/bio-eg/1-600.dna"
        "examples/glr/common/DV_lhs"
        "examples/glr/common/DaVinciTypes.hs"
        "examples/glr/packing/Main.lhs"
        "examples/glr/packing/Makefile"
        "examples/glr/packing/Expr.y"
        "examples/glr/packing/README"
        "examples/glr/packing/Hugs.lhs"
        "examples/PgnParser.ly"
        "examples/MonadTest.ly"
        "examples/igloo/ParserM.hs"
        "examples/igloo/Makefile"
        "examples/igloo/Parser.y"
        "examples/igloo/Foo.hs"
        "examples/igloo/README"
        "examples/igloo/Lexer.x"
        "examples/README"
        "examples/Calc.ly"
        "examples/DavesExample.ly"
        "examples/ErrorTest.ly"
        "examples/ErlParser.ly"
        "examples/SimonsExample.ly"
        "examples/LexerTest.ly"
        "happy.spec"
        "src/ARRAY-NOTES"
        "templates/GLR_Base.hs"
        "templates/GenericTemplate.hs"
        "templates/GLR_Lib.hs"
        "tests/AttrGrammar001.y"
        "tests/AttrGrammar002.y"
        "tests/Makefile"
        "tests/Partial.ly"
        "tests/Test.ly"
        "tests/TestMulti.ly"
        "tests/TestPrecedence.ly"
        "tests/bogus-token.y"
        "tests/bug001.ly"
        "tests/bug002.y"
        "tests/error001.stderr"
        "tests/error001.stdout"
        "tests/error001.y"
        "tests/monad001.y"
        "tests/monad002.ly"
        "tests/monaderror.y"
        "tests/precedence001.ly"
        "tests/precedence002.y"
        "tests/test_rules.y"
        "tests/issue91.y"
        "tests/issue93.y"
        "tests/issue94.y"
        "tests/issue95.y"
        "tests/monaderror-explist.y"
        "tests/typeclass_monad001.y"
        "tests/typeclass_monad002.ly"
        "tests/typeclass_monad_lexer.y"
        "tests/rank2.y"
        ];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      exes = {
        "happy" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."array" or (buildDepError "array"))
            (hsPkgs."containers" or (buildDepError "containers"))
            (hsPkgs."mtl" or (buildDepError "mtl"))
            ];
          buildable = true;
          modules = [
            "Paths_happy"
            "AbsSyn"
            "First"
            "GenUtils"
            "Grammar"
            "Info"
            "LALR"
            "Lexer"
            "ParseMonad"
            "Parser"
            "ProduceCode"
            "ProduceGLRCode"
            "NameSet"
            "Target"
            "AttrGrammar"
            "AttrGrammarParser"
            "ParamRules"
            "PrettyGrammar"
            ];
          hsSourceDirs = [ "src" ];
          mainPath = [ "Main.lhs" ];
          };
        };
      tests = {
        "tests" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."process" or (buildDepError "process"))
            ];
          buildable = true;
          mainPath = [ "test.hs" ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../.; }