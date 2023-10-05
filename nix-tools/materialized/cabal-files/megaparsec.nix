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
      identifier = { name = "megaparsec"; version = "9.2.1"; };
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
      url = "http://hackage.haskell.org/package/megaparsec-9.2.1.tar.gz";
      sha256 = "6b278397baa4ae66d613330465c919e909ced077ec308b18827b43cf6715e9ff";
      });
    }) // {
    package-description-override = "cabal-version:   2.4\r\nname:            megaparsec\r\nversion:         9.2.1\r\nx-revision: 1\r\nlicense:         BSD-2-Clause\r\nlicense-file:    LICENSE.md\r\nmaintainer:      Mark Karpov <markkarpov92@gmail.com>\r\nauthor:\r\n    Megaparsec contributors,\r\n    Paolo Martini <paolo@nemail.it>,\r\n    Daan Leijen <daan@microsoft.com>\r\n\r\ntested-with:     ghc ==8.10.7 ghc ==9.0.2 ghc ==9.2.1\r\nhomepage:        https://github.com/mrkkrp/megaparsec\r\nbug-reports:     https://github.com/mrkkrp/megaparsec/issues\r\nsynopsis:        Monadic parser combinators\r\ndescription:\r\n    This is an industrial-strength monadic parser combinator library.\r\n    Megaparsec is a feature-rich package that tries to find a nice balance\r\n    between speed, flexibility, and quality of parse errors.\r\n\r\ncategory:        Parsing\r\nbuild-type:      Simple\r\nextra-doc-files:\r\n    CHANGELOG.md\r\n    README.md\r\n\r\nsource-repository head\r\n    type:     git\r\n    location: https://github.com/mrkkrp/megaparsec.git\r\n\r\nflag dev\r\n    description: Turn on development settings.\r\n    default:     False\r\n    manual:      True\r\n\r\nlibrary\r\n    exposed-modules:\r\n        Text.Megaparsec\r\n        Text.Megaparsec.Byte\r\n        Text.Megaparsec.Byte.Binary\r\n        Text.Megaparsec.Byte.Lexer\r\n        Text.Megaparsec.Char\r\n        Text.Megaparsec.Char.Lexer\r\n        Text.Megaparsec.Debug\r\n        Text.Megaparsec.Error\r\n        Text.Megaparsec.Error.Builder\r\n        Text.Megaparsec.Internal\r\n        Text.Megaparsec.Pos\r\n        Text.Megaparsec.Stream\r\n\r\n    other-modules:\r\n        Text.Megaparsec.Class\r\n        Text.Megaparsec.Common\r\n        Text.Megaparsec.Lexer\r\n        Text.Megaparsec.State\r\n\r\n    default-language: Haskell2010\r\n    build-depends:\r\n        base >=4.13 && <5.0,\r\n        bytestring >=0.2 && <0.12,\r\n        case-insensitive >=1.2 && <1.3,\r\n        containers >=0.5 && <0.7,\r\n        deepseq >=1.3 && <1.5,\r\n        mtl >=2.2.2 && <3.0,\r\n        parser-combinators >=1.0 && <2.0,\r\n        scientific >=0.3.7 && <0.4,\r\n        text >=0.2 && <2.1,\r\n        transformers >=0.4 && <0.7\r\n\r\n    if flag(dev)\r\n        ghc-options: -O0 -Wall -Werror\r\n\r\n    else\r\n        ghc-options: -O2 -Wall\r\n\r\n    if flag(dev)\r\n        ghc-options:\r\n            -Wcompat -Wincomplete-record-updates -Wincomplete-uni-patterns\r\n            -Wnoncanonical-monad-instances -Wno-missing-home-modules\r\n\r\nbenchmark bench-speed\r\n    type:             exitcode-stdio-1.0\r\n    main-is:          Main.hs\r\n    hs-source-dirs:   bench/speed\r\n    default-language: Haskell2010\r\n    build-depends:\r\n        base >=4.13 && <5.0,\r\n        bytestring >=0.2 && <0.12,\r\n        containers >=0.5 && <0.7,\r\n        criterion >=0.6.2.1 && <1.7,\r\n        deepseq >=1.3 && <1.5,\r\n        megaparsec,\r\n        text >=0.2 && <2.1\r\n\r\n    if flag(dev)\r\n        ghc-options: -O2 -Wall -Werror\r\n\r\n    else\r\n        ghc-options: -O2 -Wall\r\n\r\nbenchmark bench-memory\r\n    type:             exitcode-stdio-1.0\r\n    main-is:          Main.hs\r\n    hs-source-dirs:   bench/memory\r\n    default-language: Haskell2010\r\n    build-depends:\r\n        base >=4.13 && <5.0,\r\n        bytestring >=0.2 && <0.12,\r\n        containers >=0.5 && <0.7,\r\n        deepseq >=1.3 && <1.5,\r\n        megaparsec,\r\n        text >=0.2 && <2.1,\r\n        weigh >=0.0.4\r\n\r\n    if flag(dev)\r\n        ghc-options: -O2 -Wall -Werror\r\n\r\n    else\r\n        ghc-options: -O2 -Wall\r\n";
    }