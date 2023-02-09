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
      identifier = { name = "tree-diff"; version = "0.0.2.1"; };
      license = "BSD-3-Clause";
      copyright = "(c) 2017-2018 Oleg Grenrus";
      maintainer = "Oleg.Grenrus <oleg.grenrus@iki.fi>";
      author = "Oleg Grenrus <oleg.grenrus@iki.fi>";
      homepage = "https://github.com/phadej/tree-diff";
      url = "";
      synopsis = "Diffing of (expression) trees.";
      description = "Common diff algorithm works on list structures:\n\n@\ndiff :: Eq a => [a] -> [a] -> [Edit a]\n@\n\nThis package works on trees.\n\n@\ntreeDiff :: Eq a => Tree a -> Tree a -> Edit (EditTree a)\n@\n\nThis package also provides a way to diff arbitrary ADTs,\nusing @Generics@-derivable helpers.\n\nThis package differs from <http://hackage.haskell.org/package/gdiff gdiff>,\nin a two ways: @tree-diff@ doesn't have patch function,\nand the \"edit-script\" is a tree itself, which is useful for pretty-printing.\n\n@\n>>> prettyEditExpr $ ediff (Foo 42 [True, False] \"old\") (Foo 42 [False, False, True] \"new\")\nFoo\n{fooBool = [-True, +False, False, +True],\nfooInt = 42,\nfooString = -\"old\" +\"new\"}\n@";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = ([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."ansi-wl-pprint" or (errorHandler.buildDepError "ansi-wl-pprint"))
          (hsPkgs."ansi-terminal" or (errorHandler.buildDepError "ansi-terminal"))
          (hsPkgs."base-compat" or (errorHandler.buildDepError "base-compat"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."generics-sop" or (errorHandler.buildDepError "generics-sop"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."MemoTrie" or (errorHandler.buildDepError "MemoTrie"))
          (hsPkgs."parsec" or (errorHandler.buildDepError "parsec"))
          (hsPkgs."parsers" or (errorHandler.buildDepError "parsers"))
          (hsPkgs."pretty" or (errorHandler.buildDepError "pretty"))
          (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
          (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
          (hsPkgs."tagged" or (errorHandler.buildDepError "tagged"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          (hsPkgs."uuid-types" or (errorHandler.buildDepError "uuid-types"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          ] ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8.0")) (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))) ++ (pkgs.lib).optionals (!(compiler.isGhc && (compiler.version).ge "7.10")) [
          (hsPkgs."void" or (errorHandler.buildDepError "void"))
          (hsPkgs."nats" or (errorHandler.buildDepError "nats"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          ];
        buildable = true;
        };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."tree-diff" or (errorHandler.buildDepError "tree-diff"))
            (hsPkgs."base-compat" or (errorHandler.buildDepError "base-compat"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."ansi-terminal" or (errorHandler.buildDepError "ansi-terminal"))
            (hsPkgs."ansi-wl-pprint" or (errorHandler.buildDepError "ansi-wl-pprint"))
            (hsPkgs."parsec" or (errorHandler.buildDepError "parsec"))
            (hsPkgs."trifecta" or (errorHandler.buildDepError "trifecta"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-golden" or (errorHandler.buildDepError "tasty-golden"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/tree-diff-0.0.2.1.tar.gz";
      sha256 = "d58fecc52b7b6a06a27b02f368538d6f7930232aac4b64e8308c91f5e994e743";
      });
    }) // {
    package-description-override = "cabal-version:       >=1.10\nname:                tree-diff\nversion:             0.0.2.1\n\nsynopsis:            Diffing of (expression) trees.\ncategory:            Data, Testing\ndescription:\n  Common diff algorithm works on list structures:\n  .\n  @\n  diff :: Eq a => [a] -> [a] -> [Edit a]\n  @\n  .\n  This package works on trees.\n  .\n  @\n  treeDiff :: Eq a => Tree a -> Tree a -> Edit (EditTree a)\n  @\n  .\n  This package also provides a way to diff arbitrary ADTs,\n  using @Generics@-derivable helpers.\n  .\n  This package differs from <http://hackage.haskell.org/package/gdiff gdiff>,\n  in a two ways: @tree-diff@ doesn't have patch function,\n  and the \"edit-script\" is a tree itself, which is useful for pretty-printing.\n  .\n  @\n  >>> prettyEditExpr $ ediff (Foo 42 [True, False] \"old\") (Foo 42 [False, False, True] \"new\")\n  Foo\n    {fooBool = [-True, +False, False, +True],\n     fooInt = 42,\n     fooString = -\"old\" +\"new\"}\n  @\nhomepage:            https://github.com/phadej/tree-diff\nbug-reports:         https://github.com/phadej/tree-diff/issues\nlicense:             BSD3\nlicense-file:        LICENSE\nauthor:              Oleg Grenrus <oleg.grenrus@iki.fi>\nmaintainer:          Oleg.Grenrus <oleg.grenrus@iki.fi>\ncopyright:           (c) 2017-2018 Oleg Grenrus\nbuild-type:          Simple\nextra-source-files:  README.md ChangeLog.md\ntested-with:\n  GHC==7.8.4,\n  GHC==7.10.3,\n  GHC==8.0.2,\n  GHC==8.2.2,\n  GHC==8.4.4,\n  GHc==8.6.4\n\nextra-source-files:\n  fixtures/exfoo.expr\n\nsource-repository head\n  type:      git\n  location:  https://github.com/phadej/tree-diff.git\n\nlibrary\n  exposed-modules:\n    Data.TreeDiff\n    Data.TreeDiff.List\n    Data.TreeDiff.Tree\n    Data.TreeDiff.Expr\n    Data.TreeDiff.Class\n    Data.TreeDiff.Pretty\n    Data.TreeDiff.Parser\n    Data.TreeDiff.Golden\n    Data.TreeDiff.QuickCheck\n  build-depends:\n    base                 >=4.7      && <4.13,\n    aeson                >=1.2.1.0  && <1.5,\n    ansi-wl-pprint       >=0.6.8.1  && <0.7,\n    ansi-terminal        >=0.6.3.1  && <0.10,\n    base-compat          >=0.9.3    && <0.11,\n    bytestring           >=0.10.4.0 && <0.11,\n    containers           >=0.5.5.1  && <0.7,\n    generics-sop         >=0.3.1.0  && <0.6,\n    hashable             >=1.2.6.1  && <1.4,\n    MemoTrie             >=0.6.8    && <0.7,\n    parsec               >=3.1.11   && <3.2,\n    parsers              >=0.12.7   && <0.13,\n    pretty               >=1.1.1.1  && <1.2,\n    QuickCheck           >=2.10.0.1 && <2.14,\n    scientific           >=0.3.5.2  && <0.4,\n    tagged               >=0.8.5    && <0.9,\n    text                 >=1.2.2.2  && <1.3,\n    time                 >=1.4.2    && <1.9,\n    unordered-containers >=0.2.8.0  && <0.3,\n    uuid-types           >=1.0.3    && <1.1,\n    vector               >=0.12     && <0.13\n\n  if !impl(ghc >= 8.0)\n    build-depends:\n      semigroups         >=0.18.3   && <0.20\n\n  if !impl(ghc >= 7.10)\n    build-depends:\n      void               >=0.7.2    && <0.8,\n      nats               >=1.1.1    && <1.2,\n      transformers       >=0.3.0.0  && <0.6\n\n  other-extensions:\n    ConstraintKinds\n    CPP\n    DefaultSignatures\n    FlexibleContexts\n    GADTs\n    RankNTypes\n    ScopedTypeVariables\n  hs-source-dirs:      src\n  default-language:    Haskell2010\n\ntest-suite test\n  default-language:    Haskell2010\n  type:                exitcode-stdio-1.0\n  main-is:             Tests.hs\n  hs-source-dirs:      tests\n  ghc-options:         -Wall -threaded\n  build-depends:\n    base, tree-diff,\n    base-compat,\n    QuickCheck,\n    ansi-terminal,\n    ansi-wl-pprint,\n    parsec,\n    trifecta             >=1.7.1.1  && <2.1,\n    tasty                >=0.11.2.5 && <1.3,\n    tasty-golden         >=2.3.1.1  && <2.4,\n    tasty-quickcheck     >=0.9.1    && <0.11\n";
    }