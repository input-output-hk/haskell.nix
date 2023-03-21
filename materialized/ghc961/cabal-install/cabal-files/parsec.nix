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
      specVersion = "1.12";
      identifier = { name = "parsec"; version = "3.1.16.1"; };
      license = "BSD-2-Clause";
      copyright = "";
      maintainer = "Oleg Grenrus <oleg.grenrus@iki.fi>, Herbert Valerio Riedel <hvr@gnu.org>";
      author = "Daan Leijen <daan@microsoft.com>, Paolo Martini <paolo@nemail.it>, Antoine Latter <aslatter@gmail.com>";
      homepage = "https://github.com/haskell/parsec";
      url = "";
      synopsis = "Monadic parser combinators";
      description = "Parsec is designed from scratch as an industrial-strength parser\nlibrary.  It is simple, safe, well documented (on the package\nhomepage), has extensive libraries, good error messages,\nand is fast.  It is defined as a monad transformer that can be\nstacked on arbitrary monads, and it is also parametric in the\ninput stream type.\n\nThe main entry point is the \"Text.Parsec\" module which provides\ndefaults for parsing 'Char'acter data.\n\nThe \"Text.ParserCombinators.Parsec\" module hierarchy contains\nthe legacy @parsec-2@ API and may be removed at some point in\nthe future.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          ] ++ (pkgs.lib).optionals (!(compiler.isGhc && (compiler.version).ge "8.0")) [
          (hsPkgs."fail" or (errorHandler.buildDepError "fail"))
          (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
          ];
        buildable = true;
        };
      tests = {
        "parsec-tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."parsec" or (errorHandler.buildDepError "parsec"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            ] ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8.0")) (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"));
          buildable = true;
          };
        "parsec-issue127" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."parsec" or (errorHandler.buildDepError "parsec"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/parsec-3.1.16.1.tar.gz";
      sha256 = "a41962e5d76ea68658876735b8d5b755e0eff336b079d0a2f439c364755d1246";
      });
    }) // {
    package-description-override = "cabal-version:  1.12\nname:           parsec\nversion:        3.1.16.1\n\nsynopsis:       Monadic parser combinators\ndescription:    Parsec is designed from scratch as an industrial-strength parser\n                library.  It is simple, safe, well documented (on the package\n                homepage), has extensive libraries, good error messages,\n                and is fast.  It is defined as a monad transformer that can be\n                stacked on arbitrary monads, and it is also parametric in the\n                input stream type.\n                .\n                The main entry point is the \"Text.Parsec\" module which provides\n                defaults for parsing 'Char'acter data.\n                .\n                The \"Text.ParserCombinators.Parsec\" module hierarchy contains\n                the legacy @parsec-2@ API and may be removed at some point in\n                the future.\n\nlicense:        BSD2\nlicense-file:   LICENSE\nauthor:         Daan Leijen <daan@microsoft.com>, Paolo Martini <paolo@nemail.it>, Antoine Latter <aslatter@gmail.com>\nmaintainer:     Oleg Grenrus <oleg.grenrus@iki.fi>, Herbert Valerio Riedel <hvr@gnu.org>\nhomepage:       https://github.com/haskell/parsec\nbug-reports:    https://github.com/haskell/parsec/issues\ncategory:       Parsing\n\nbuild-type:     Simple\ntested-with:    GHC ==9.2.2 || ==9.0.2 || ==8.10.7 || ==8.8.4 || ==8.6.5 || ==8.4.4 || ==8.2.2 || ==8.0.2 || ==7.10.3 || ==7.8.4 || ==7.6.3 || ==7.4.2\n\nextra-source-files: ChangeLog.md, README.md\n\nsource-repository head\n    type: git\n    location: https://github.com/haskell/parsec\n\nlibrary\n    hs-source-dirs: src\n    exposed-modules:\n        Text.Parsec\n        Text.Parsec.String\n        Text.Parsec.ByteString\n        Text.Parsec.ByteString.Lazy\n        Text.Parsec.Text\n        Text.Parsec.Text.Lazy\n        Text.Parsec.Pos\n        Text.Parsec.Error\n        Text.Parsec.Prim\n        Text.Parsec.Char\n        Text.Parsec.Combinator\n        Text.Parsec.Token\n        Text.Parsec.Expr\n        Text.Parsec.Language\n        Text.Parsec.Perm\n        Text.ParserCombinators.Parsec\n        Text.ParserCombinators.Parsec.Char\n        Text.ParserCombinators.Parsec.Combinator\n        Text.ParserCombinators.Parsec.Error\n        Text.ParserCombinators.Parsec.Expr\n        Text.ParserCombinators.Parsec.Language\n        Text.ParserCombinators.Parsec.Perm\n        Text.ParserCombinators.Parsec.Pos\n        Text.ParserCombinators.Parsec.Prim\n        Text.ParserCombinators.Parsec.Token\n\n    build-depends:\n        base       >= 4.5.1.0 && < 4.19,\n        mtl        >= 2.1.3.1 && < 2.4,\n        bytestring >= 0.9.2.1 && < 0.12,\n        text      (>= 1.2.3.0  && < 1.3)\n               || (>= 2.0 && < 2.1)\n\n    default-language: Haskell2010\n    other-extensions:\n        CPP\n        DeriveDataTypeable\n        ExistentialQuantification\n        FlexibleContexts\n        FlexibleInstances\n        FunctionalDependencies\n        MultiParamTypeClasses\n        PolymorphicComponents\n        StandaloneDeriving\n        Safe\n        Trustworthy\n        UndecidableInstances\n\n    ghc-options: -Wall\n    if impl(ghc >= 8.0)\n        ghc-options: -Wcompat -Wnoncanonical-monad-instances -Wno-trustworthy-safe\n        if impl(ghc < 8.8)\n          ghc-options: -Wnoncanonical-monadfail-instances\n    else\n        -- provide/emulate `Control.Monad.Fail` and `Semigroup` API for pre-GHC8\n        build-depends: fail == 4.9.*, semigroups >= 0.18 && < 0.21\n\n        if impl(ghc >= 7.10)\n            ghc-options: -fno-warn-trustworthy-safe\n\n    -- these flags may abort compilation with GHC-8.10\n    -- https://gitlab.haskell.org/ghc/ghc/-/merge_requests/3295\n    -- https://gitlab.haskell.org/ghc/ghc/-/issues/22728\n    -- if impl(ghc >= 9.0)\n    --    -- ghc-options: -Winferred-safe-imports -Wmissing-safe-haskell-mode\n\ntest-suite parsec-tests\n    type: exitcode-stdio-1.0\n\n    hs-source-dirs: test\n    main-is: Main.hs\n    other-modules:\n        Bugs\n        Bugs.Bug2\n        Bugs.Bug6\n        Bugs.Bug9\n        Bugs.Bug35\n        Features\n        Features.Feature80\n        Features.Feature150\n        Util\n\n    build-depends:\n        base,\n        mtl,\n        parsec,\n        -- dependencies whose version bounds are not inherited via lib:parsec\n        tasty >= 1.4 && < 1.5,\n        tasty-hunit >= 0.10 && < 0.11\n\n    default-language: Haskell2010\n\n    ghc-options: -Wall\n    if impl(ghc >= 8.0)\n        ghc-options: -Wcompat -Wnoncanonical-monad-instances -Wnoncanonical-monadfail-instances\n    else\n        build-depends: semigroups\n\ntest-suite parsec-issue127\n    default-language: Haskell2010\n    type: exitcode-stdio-1.0\n    main-is: issue127.hs\n    hs-source-dirs: test\n    build-depends: base, parsec\n";
    }