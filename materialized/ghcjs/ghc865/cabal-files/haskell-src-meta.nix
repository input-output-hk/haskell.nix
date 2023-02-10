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
      identifier = { name = "haskell-src-meta"; version = "0.8.7"; };
      license = "BSD-3-Clause";
      copyright = "(c) Matt Morrow";
      maintainer = "danburton.email@gmail.com";
      author = "Matt Morrow";
      homepage = "";
      url = "";
      synopsis = "Parse source to template-haskell abstract syntax.";
      description = "The translation from haskell-src-exts abstract syntax\nto template-haskell abstract syntax isn't 100% complete yet.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."haskell-src-exts" or (errorHandler.buildDepError "haskell-src-exts"))
          (hsPkgs."pretty" or (errorHandler.buildDepError "pretty"))
          (hsPkgs."syb" or (errorHandler.buildDepError "syb"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."th-orphans" or (errorHandler.buildDepError "th-orphans"))
          ] ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).lt "7.8") (hsPkgs."safe" or (errorHandler.buildDepError "safe"));
        buildable = true;
        };
      tests = {
        "unit" = {
          depends = [
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."haskell-src-exts" or (errorHandler.buildDepError "haskell-src-exts"))
            (hsPkgs."haskell-src-meta" or (errorHandler.buildDepError "haskell-src-meta"))
            (hsPkgs."pretty" or (errorHandler.buildDepError "pretty"))
            (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            ] ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8.0")) (hsPkgs."fail" or (errorHandler.buildDepError "fail"));
          buildable = true;
          };
        "splices" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."haskell-src-exts" or (errorHandler.buildDepError "haskell-src-exts"))
            (hsPkgs."haskell-src-meta" or (errorHandler.buildDepError "haskell-src-meta"))
            (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
            ];
          buildable = true;
          };
        "examples" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."haskell-src-meta" or (errorHandler.buildDepError "haskell-src-meta"))
            (hsPkgs."pretty" or (errorHandler.buildDepError "pretty"))
            (hsPkgs."syb" or (errorHandler.buildDepError "syb"))
            (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
            ] ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8.0")) (hsPkgs."fail" or (errorHandler.buildDepError "fail"));
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/haskell-src-meta-0.8.7.tar.gz";
      sha256 = "361ad6f1ff47c89228e3b1e7beff52a5940484480f09d847c73fdc18966bc2fb";
      });
    }) // {
    package-description-override = "name:               haskell-src-meta\nversion:            0.8.7\ncabal-version:      >= 1.10\nbuild-type:         Simple\nlicense:            BSD3\nlicense-file:       LICENSE\ncategory:           Language, Template Haskell\nauthor:             Matt Morrow\ncopyright:          (c) Matt Morrow\nmaintainer:         danburton.email@gmail.com\nbug-reports:        https://github.com/DanBurton/haskell-src-meta/issues\ntested-with:        GHC == 7.10.3, GHC == 8.0.2, GHC == 8.2.2, GHC == 8.4.4, GHC == 8.6.5, GHC == 8.8.1\nsynopsis:           Parse source to template-haskell abstract syntax.\ndescription:        The translation from haskell-src-exts abstract syntax\n                    to template-haskell abstract syntax isn't 100% complete yet.\n\nextra-source-files: ChangeLog README.md\n\nlibrary\n  default-language: Haskell2010\n  build-depends:   base >= 4.8 && < 5,\n                   haskell-src-exts >= 1.18 && < 1.24,\n                   pretty >= 1.0 && < 1.2,\n                   syb >= 0.1 && < 0.8,\n                   template-haskell >= 2.10 && < 2.18,\n                   th-orphans >= 0.12 && < 0.14\n\n  if impl(ghc < 7.8)\n    build-depends: safe <= 0.3.9\n\n  hs-source-dirs:  src\n  exposed-modules: Language.Haskell.Meta\n                   Language.Haskell.Meta.Parse\n                   Language.Haskell.Meta.Syntax.Translate\n                   Language.Haskell.Meta.Utils\n\ntest-suite unit\n  default-language: Haskell2010\n  type:             exitcode-stdio-1.0\n  hs-source-dirs:   tests\n  main-is:          Main.hs\n\n  build-depends:\n    HUnit                >= 1.2,\n    base                 >= 4.5,\n    haskell-src-exts     >= 1.17,\n    haskell-src-meta,\n    pretty               >= 1.0,\n    template-haskell     >= 2.10,\n    tasty,\n    tasty-hunit\n\n  -- this is needed to access Control.Monad.Fail on GHCs before 8.0\n  if !impl(ghc >= 8.0)\n    Build-Depends:\n      fail == 4.9.*\n\n\ntest-suite splices\n  default-language: Haskell2010\n  type:             exitcode-stdio-1.0\n  hs-source-dirs:   tests\n  main-is:          Splices.hs\n\n  build-depends:\n    base,\n    haskell-src-exts,\n    haskell-src-meta,\n    template-haskell\n\ntest-suite examples\n  default-language: Haskell2010\n  type:             exitcode-stdio-1.0\n  hs-source-dirs:   examples, tests\n  main-is:          TestExamples.hs\n\n  build-depends:\n    base,\n    containers,\n    haskell-src-meta,\n    pretty,\n    syb,\n    template-haskell\n\n  -- this is needed to access Control.Monad.Fail on GHCs before 8.0\n  if !impl(ghc >= 8.0)\n    Build-Depends:\n      fail == 4.9.*\n\n\n  other-modules:\n    BF,\n    Hs,\n    HsHere,\n    SKI\n\nsource-repository head\n  type:     git\n  location: git://github.com/danburton/haskell-src-meta.git\n";
    }