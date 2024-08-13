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
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = { name = "alex"; version = "3.2.7.1"; };
      license = "BSD-3-Clause";
      copyright = "(c) Chis Dornan, Simon Marlow";
      maintainer = "Simon Marlow <marlowsd@gmail.com>";
      author = "Chris Dornan and Simon Marlow";
      homepage = "http://www.haskell.org/alex/";
      url = "";
      synopsis = "Alex is a tool for generating lexical analysers in Haskell";
      description = "Alex is a tool for generating lexical analysers in Haskell.\nIt takes a description of tokens based on regular\nexpressions and generates a Haskell module containing code\nfor scanning text efficiently. It is similar to the tool\nlex or flex for C/C++.";
      buildType = "Simple";
      };
    components = {
      exes = {
        "alex" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."array" or (errorHandler.buildDepError "array"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            ];
          buildable = true;
          };
        };
      tests = {
        "tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."process" or (errorHandler.buildDepError "process"))
            ];
          build-tools = [
            (hsPkgs.pkgsBuildBuild.alex.components.exes.alex or (pkgs.pkgsBuildBuild.alex or (errorHandler.buildToolDepError "alex:alex")))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/alex-3.2.7.1.tar.gz";
      sha256 = "9bd2f1a27e8f1b2ffdb5b2fbd3ed82b6f0e85191459a1b24ffcbef4e68a81bec";
      });
    }) // {
    package-description-override = "cabal-version: >= 1.10\nname: alex\nversion: 3.2.7.1\n-- don't forget updating changelog.md!\nlicense: BSD3\nlicense-file: LICENSE\ncopyright: (c) Chis Dornan, Simon Marlow\nauthor: Chris Dornan and Simon Marlow\nmaintainer: Simon Marlow <marlowsd@gmail.com>\nbug-reports: https://github.com/simonmar/alex/issues\nstability: stable\nhomepage: http://www.haskell.org/alex/\nsynopsis: Alex is a tool for generating lexical analysers in Haskell\ndescription:\n  Alex is a tool for generating lexical analysers in Haskell.\n  It takes a description of tokens based on regular\n  expressions and generates a Haskell module containing code\n  for scanning text efficiently. It is similar to the tool\n  lex or flex for C/C++.\n\ncategory: Development\nbuild-type: Simple\n\ntested-with:\n        GHC == 7.0.4\n        GHC == 7.4.2\n        GHC == 7.6.3\n        GHC == 7.8.4\n        GHC == 7.10.3\n        GHC == 8.0.2\n        GHC == 8.2.2\n        GHC == 8.4.4\n        GHC == 8.6.5\n        GHC == 8.8.4\n        GHC == 8.10.4\n        GHC == 9.0.1\n\ndata-dir: data/\n\ndata-files:\n        AlexTemplate.hs\n        AlexWrappers.hs\n\nextra-source-files:\n        CHANGELOG.md\n        README.md\n        TODO\n        doc/Makefile\n        doc/aclocal.m4\n        doc/alex.1.in\n        doc/alex.xml\n        doc/config.mk.in\n        doc/configure.ac\n        doc/docbook-xml.mk\n        doc/fptools.css\n        examples/Makefile\n        examples/Tokens.x\n        examples/Tokens_gscan.x\n        examples/Tokens_posn.x\n        examples/examples.x\n        examples/haskell.x\n        examples/lit.x\n        examples/pp.x\n        examples/state.x\n        examples/tiny.y\n        examples/words.x\n        examples/words_monad.x\n        examples/words_posn.x\n        src/Parser.y.boot\n        src/Scan.x.boot\n        src/ghc_hooks.c\n        tests/Makefile\n        tests/simple.x\n        tests/null.x\n        tests/tokens.x\n        tests/tokens_gscan.x\n        tests/tokens_posn.x\n        tests/tokens_bytestring.x\n        tests/tokens_posn_bytestring.x\n        tests/tokens_scan_user.x\n        tests/tokens_strict_bytestring.x\n        tests/tokens_monad_bytestring.x\n        tests/tokens_monadUserState_bytestring.x\n        tests/tokens_bytestring_unicode.x\n        tests/basic_typeclass.x\n        tests/basic_typeclass_bytestring.x\n        tests/default_typeclass.x\n        tests/gscan_typeclass.x\n        tests/posn_typeclass.x\n        tests/monad_typeclass.x\n        tests/monad_typeclass_bytestring.x\n        tests/monadUserState_typeclass.x\n        tests/monadUserState_typeclass_bytestring.x\n        tests/posn_typeclass_bytestring.x\n        tests/strict_typeclass.x\n        tests/unicode.x\n        tests/issue_71.x\n        tests/issue_119.x\n        tests/issue_141.x\n        tests/issue_197.x\n\nsource-repository head\n    type:     git\n    location: https://github.com/simonmar/alex.git\n\nexecutable alex\n  hs-source-dirs: src\n  main-is: Main.hs\n\n  build-depends: base >= 2.1 && < 5\n               , array\n               , containers\n               , directory\n\n  default-language: Haskell98\n  default-extensions: CPP\n  other-extensions: MagicHash\n\n  ghc-options: -Wall -rtsopts\n\n  other-modules:\n        AbsSyn\n        CharSet\n        DFA\n        DFAMin\n        DFS\n        Info\n        Map\n        NFA\n        Output\n        Paths_alex\n        Parser\n        ParseMonad\n        Scan\n        Set\n        Sort\n        Util\n        UTF8\n        Data.Ranged\n        Data.Ranged.Boundaries\n        Data.Ranged.RangedSet\n        Data.Ranged.Ranges\n\ntest-suite tests\n  type: exitcode-stdio-1.0\n  main-is: test.hs\n  -- This line is important as it ensures that the local `exe:alex` component declared above is built before the test-suite component is invoked, as well as making sure that `alex` is made available on $PATH and `$alex_datadir` is set accordingly before invoking `test.hs`\n  build-tools: alex\n\n  default-language: Haskell98\n\n  build-depends: base, process\n";
    }