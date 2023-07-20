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
    flags = { dev = false; };
    package = {
      specVersion = "2.4";
      identifier = { name = "megaparsec"; version = "9.2.2"; };
      license = "BSD-2-Clause";
      copyright = "";
      maintainer = "Mark Karpov <markkarpov92@gmail.com>";
      author = "Megaparsec contributors,\nPaolo Martini <paolo@nemail.it>,\nDaan Leijen <daan@microsoft.com>";
      homepage = "https://github.com/mrkkrp/megaparsec";
      url = "";
      synopsis = "Monadic parser combinators";
      description = "This is an industrial-strength monadic parser combinator library.\nMegaparsec is a feature-rich package that tries to find a nice balance\nbetween speed, flexibility, and quality of parse errors.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."case-insensitive" or (errorHandler.buildDepError "case-insensitive"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."parser-combinators" or (errorHandler.buildDepError "parser-combinators"))
          (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          ];
        buildable = true;
        };
      benchmarks = {
        "bench-speed" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."megaparsec" or (errorHandler.buildDepError "megaparsec"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            ];
          buildable = true;
          };
        "bench-memory" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."megaparsec" or (errorHandler.buildDepError "megaparsec"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."weigh" or (errorHandler.buildDepError "weigh"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/megaparsec-9.2.2.tar.gz";
      sha256 = "a48b03e55c0b9efcc88ce4236186fb6eee3acb64336987dd9c98daf4d96aa234";
      });
    }) // {
    package-description-override = "cabal-version:   2.4\nname:            megaparsec\nversion:         9.2.2\nlicense:         BSD-2-Clause\nlicense-file:    LICENSE.md\nmaintainer:      Mark Karpov <markkarpov92@gmail.com>\nauthor:\n    Megaparsec contributors,\n    Paolo Martini <paolo@nemail.it>,\n    Daan Leijen <daan@microsoft.com>\n\ntested-with:     ghc ==9.0.2 ghc ==9.2.4 ghc ==9.4.1\nhomepage:        https://github.com/mrkkrp/megaparsec\nbug-reports:     https://github.com/mrkkrp/megaparsec/issues\nsynopsis:        Monadic parser combinators\ndescription:\n    This is an industrial-strength monadic parser combinator library.\n    Megaparsec is a feature-rich package that tries to find a nice balance\n    between speed, flexibility, and quality of parse errors.\n\ncategory:        Parsing\nbuild-type:      Simple\nextra-doc-files:\n    CHANGELOG.md\n    README.md\n\nsource-repository head\n    type:     git\n    location: https://github.com/mrkkrp/megaparsec.git\n\nflag dev\n    description: Turn on development settings.\n    default:     False\n    manual:      True\n\nlibrary\n    exposed-modules:\n        Text.Megaparsec\n        Text.Megaparsec.Byte\n        Text.Megaparsec.Byte.Binary\n        Text.Megaparsec.Byte.Lexer\n        Text.Megaparsec.Char\n        Text.Megaparsec.Char.Lexer\n        Text.Megaparsec.Debug\n        Text.Megaparsec.Error\n        Text.Megaparsec.Error.Builder\n        Text.Megaparsec.Internal\n        Text.Megaparsec.Pos\n        Text.Megaparsec.Stream\n\n    other-modules:\n        Text.Megaparsec.Class\n        Text.Megaparsec.Common\n        Text.Megaparsec.Lexer\n        Text.Megaparsec.State\n\n    default-language: Haskell2010\n    build-depends:\n        base >=4.15 && <5.0,\n        bytestring >=0.2 && <0.12,\n        case-insensitive >=1.2 && <1.3,\n        containers >=0.5 && <0.7,\n        deepseq >=1.3 && <1.5,\n        mtl >=2.2.2 && <3.0,\n        parser-combinators >=1.0 && <2.0,\n        scientific >=0.3.7 && <0.4,\n        text >=0.2 && <2.1,\n        transformers >=0.4 && <0.7\n\n    if flag(dev)\n        ghc-options: -O0 -Wall -Werror\n\n    else\n        ghc-options: -O2 -Wall\n\n    if flag(dev)\n        ghc-options:\n            -Wcompat -Wincomplete-record-updates -Wincomplete-uni-patterns\n            -Wnoncanonical-monad-instances -Wno-missing-home-modules\n\nbenchmark bench-speed\n    type:             exitcode-stdio-1.0\n    main-is:          Main.hs\n    hs-source-dirs:   bench/speed\n    default-language: Haskell2010\n    build-depends:\n        base >=4.15 && <5.0,\n        bytestring >=0.2 && <0.12,\n        containers >=0.5 && <0.7,\n        criterion >=0.6.2.1 && <1.7,\n        deepseq >=1.3 && <1.5,\n        megaparsec,\n        text >=0.2 && <2.1\n\n    if flag(dev)\n        ghc-options: -O2 -Wall -Werror\n\n    else\n        ghc-options: -O2 -Wall\n\nbenchmark bench-memory\n    type:             exitcode-stdio-1.0\n    main-is:          Main.hs\n    hs-source-dirs:   bench/memory\n    default-language: Haskell2010\n    build-depends:\n        base >=4.15 && <5.0,\n        bytestring >=0.2 && <0.12,\n        containers >=0.5 && <0.7,\n        deepseq >=1.3 && <1.5,\n        megaparsec,\n        text >=0.2 && <2.1,\n        weigh >=0.0.4\n\n    if flag(dev)\n        ghc-options: -O2 -Wall -Werror\n\n    else\n        ghc-options: -O2 -Wall\n";
    }