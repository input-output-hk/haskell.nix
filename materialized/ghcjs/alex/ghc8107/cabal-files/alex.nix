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
    flags = { small_base = true; };
    package = {
      specVersion = "1.8";
      identifier = { name = "alex"; version = "3.2.5"; };
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
          ] ++ (if flags.small_base
            then [
              (hsPkgs."base" or (errorHandler.buildDepError "base"))
              (hsPkgs."array" or (errorHandler.buildDepError "array"))
              (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
              (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            ]
            else [ (hsPkgs."base" or (errorHandler.buildDepError "base")) ]);
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
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/alex-3.2.5.tar.gz";
      sha256 = "b77c8a1270767c64e2adb21a6e91ee7cd904ba17edae17bc20fd03da5256e0e3";
    });
  }) // {
    package-description-override = "cabal-version: >= 1.8\r\nname: alex\r\nversion: 3.2.5\r\nx-revision: 1\r\n-- don't forget updating changelog.md!\r\nlicense: BSD3\r\nlicense-file: LICENSE\r\ncopyright: (c) Chis Dornan, Simon Marlow\r\nauthor: Chris Dornan and Simon Marlow\r\nmaintainer: Simon Marlow <marlowsd@gmail.com>\r\nbug-reports: https://github.com/simonmar/alex/issues\r\nstability: stable\r\nhomepage: http://www.haskell.org/alex/\r\nsynopsis: Alex is a tool for generating lexical analysers in Haskell\r\ndescription:\r\n  Alex is a tool for generating lexical analysers in Haskell.\r\n  It takes a description of tokens based on regular\r\n  expressions and generates a Haskell module containing code\r\n  for scanning text efficiently. It is similar to the tool\r\n  lex or flex for C/C++.\r\n\r\ncategory: Development\r\nbuild-type: Simple\r\n\r\ndata-dir: data/\r\n\r\ndata-files:\r\n        AlexTemplate\r\n        AlexTemplate-ghc\r\n        AlexTemplate-ghc-nopred\r\n        AlexTemplate-ghc-debug\r\n        AlexTemplate-debug\r\n        AlexWrapper-basic\r\n        AlexWrapper-basic-bytestring\r\n        AlexWrapper-strict-bytestring\r\n        AlexWrapper-posn\r\n        AlexWrapper-posn-bytestring\r\n        AlexWrapper-monad\r\n        AlexWrapper-monad-bytestring\r\n        AlexWrapper-monadUserState\r\n        AlexWrapper-monadUserState-bytestring\r\n        AlexWrapper-gscan\r\n\r\nextra-source-files:\r\n        CHANGELOG.md\r\n        README.md\r\n        TODO\r\n        alex.spec\r\n        doc/Makefile\r\n        doc/aclocal.m4\r\n        doc/alex.1.in\r\n        doc/alex.xml\r\n        doc/config.mk.in\r\n        doc/configure.ac\r\n        doc/docbook-xml.mk\r\n        doc/fptools.css\r\n        examples/Makefile\r\n        examples/Tokens.x\r\n        examples/Tokens_gscan.x\r\n        examples/Tokens_posn.x\r\n        examples/examples.x\r\n        examples/haskell.x\r\n        examples/lit.x\r\n        examples/pp.x\r\n        examples/state.x\r\n        examples/tiny.y\r\n        examples/words.x\r\n        examples/words_monad.x\r\n        examples/words_posn.x\r\n        src/Parser.y.boot\r\n        src/Scan.x.boot\r\n        src/ghc_hooks.c\r\n        templates/GenericTemplate.hs\r\n        templates/wrappers.hs\r\n        tests/Makefile\r\n        tests/simple.x\r\n        tests/null.x\r\n        tests/tokens.x\r\n        tests/tokens_gscan.x\r\n        tests/tokens_posn.x\r\n        tests/tokens_bytestring.x\r\n        tests/tokens_posn_bytestring.x\r\n        tests/tokens_scan_user.x\r\n        tests/tokens_strict_bytestring.x\r\n        tests/tokens_monad_bytestring.x\r\n        tests/tokens_monadUserState_bytestring.x\r\n        tests/tokens_bytestring_unicode.x\r\n        tests/basic_typeclass.x\r\n        tests/basic_typeclass_bytestring.x\r\n        tests/default_typeclass.x\r\n        tests/gscan_typeclass.x\r\n        tests/posn_typeclass.x\r\n        tests/monad_typeclass.x\r\n        tests/monad_typeclass_bytestring.x\r\n        tests/monadUserState_typeclass.x\r\n        tests/monadUserState_typeclass_bytestring.x\r\n        tests/posn_typeclass_bytestring.x\r\n        tests/strict_typeclass.x\r\n        tests/unicode.x\r\n\r\nsource-repository head\r\n    type:     git\r\n    location: https://github.com/simonmar/alex.git\r\n\r\nflag small_base\r\n  description: Choose the new smaller, split-up base package.\r\n\r\nexecutable alex\r\n  hs-source-dirs: src\r\n  main-is: Main.hs\r\n\r\n  if flag(small_base)\r\n    build-depends: base >= 2.1, array, containers, directory\r\n  else\r\n    build-depends: base >= 1.0\r\n\r\n  build-depends: base < 4.15.0.0\r\n\r\n  extensions: CPP\r\n  ghc-options: -Wall -rtsopts\r\n  other-modules:\r\n        AbsSyn\r\n        CharSet\r\n        DFA\r\n        DFAMin\r\n        DFS\r\n        Info\r\n        Map\r\n        NFA\r\n        Output\r\n        Paths_alex\r\n        Parser\r\n        ParseMonad\r\n        Scan\r\n        Set\r\n        Sort\r\n        Util\r\n        UTF8\r\n        Data.Ranged\r\n        Data.Ranged.Boundaries\r\n        Data.Ranged.RangedSet\r\n        Data.Ranged.Ranges\r\n\r\ntest-suite tests\r\n  type: exitcode-stdio-1.0\r\n  main-is: test.hs\r\n  -- This line is important as it ensures that the local `exe:alex` component declared above is built before the test-suite component is invoked, as well as making sure that `alex` is made available on $PATH and `$alex_datadir` is set accordingly before invoking `test.hs`\r\n  build-tools: alex\r\n\r\n  build-depends: base, process\r\n";
  }