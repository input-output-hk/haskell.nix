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
      identifier = { name = "haskell-src-exts"; version = "1.20.3"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Matthew Pickering <matthewtpickering@gmail.com>";
      author = "Niklas Broberg";
      homepage = "https://github.com/haskell-suite/haskell-src-exts";
      url = "";
      synopsis = "Manipulating Haskell source: abstract syntax, lexer, parser, and pretty-printer";
      description = "Haskell-Source with Extensions (HSE, haskell-src-exts)\nis a standalone parser for Haskell. In addition to\nstandard Haskell, all extensions implemented in GHC are supported.\n\nApart from these standard extensions,\nit also handles regular patterns as per the HaRP extension\nas well as HSX-style embedded XML syntax.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          (hsPkgs."pretty" or (errorHandler.buildDepError "pretty"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
          ] ++ (pkgs.lib).optionals (!(compiler.isGhc && (compiler.version).ge "8.0")) [
          (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
          (hsPkgs."fail" or (errorHandler.buildDepError "fail"))
          ];
        build-tools = [
          (hsPkgs.buildPackages.happy.components.exes.happy or (pkgs.buildPackages.happy or (errorHandler.buildToolDepError "happy:happy")))
          ];
        buildable = true;
        };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."haskell-src-exts" or (errorHandler.buildDepError "haskell-src-exts"))
            (hsPkgs."smallcheck" or (errorHandler.buildDepError "smallcheck"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-smallcheck" or (errorHandler.buildDepError "tasty-smallcheck"))
            (hsPkgs."tasty-golden" or (errorHandler.buildDepError "tasty-golden"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."pretty-show" or (errorHandler.buildDepError "pretty-show"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/haskell-src-exts-1.20.3.tar.gz";
      sha256 = "433e68a731fb6a1435e86d3eb3b2878db9c5d51dc1f7499d85bbf5ac3ed1e4a8";
      });
    }) // {
    package-description-override = "Name:                   haskell-src-exts\r\nVersion:                1.20.3\r\nx-revision: 1\r\nLicense:                BSD3\r\nLicense-File:           LICENSE\r\nBuild-Type:             Simple\r\nAuthor:                 Niklas Broberg\r\nMaintainer:             Matthew Pickering <matthewtpickering@gmail.com>\r\nCategory:               Language\r\nSynopsis:               Manipulating Haskell source: abstract syntax, lexer, parser, and pretty-printer\r\nDescription:            Haskell-Source with Extensions (HSE, haskell-src-exts)\r\n                        is a standalone parser for Haskell. In addition to\r\n                        standard Haskell, all extensions implemented in GHC are supported.\r\n                        .\r\n                        Apart from these standard extensions,\r\n                        it also handles regular patterns as per the HaRP extension\r\n                        as well as HSX-style embedded XML syntax.\r\nHomepage:               https://github.com/haskell-suite/haskell-src-exts\r\nStability:              Stable\r\nCabal-Version:          >= 1.10\r\nTested-With:\r\n                        GHC == 7.6.3\r\n                        , GHC == 7.8.2\r\n                        , GHC == 7.10.3\r\n                        , GHC == 8.0.2\r\n                        , GHC == 8.2.2\r\n                        , GHC == 8.4.1\r\n                        , GHC == 8.6.1\r\n\r\nExtra-Source-Files:\r\n                        README.md\r\n                        CHANGELOG\r\n                        RELEASENOTES-1.17.0\r\n                        tests/examples/*.hs\r\n                        tests/examples/*.lhs\r\n                        tests/examples/*.hs.parser.golden\r\n                        tests/examples/*.lhs.parser.golden\r\n                        tests/examples/*.hs.exactprinter.golden\r\n                        tests/examples/*.lhs.exactprinter.golden\r\n                        tests/examples/*.hs.prettyprinter.golden\r\n                        tests/examples/*.lhs.prettyprinter.golden\r\n                        tests/examples/*.hs.prettyparser.golden\r\n                        tests/examples/*.lhs.prettyparser.golden\r\n                        tests/Runner.hs\r\n                        tests/Extensions.hs\r\n\r\nLibrary\r\n  Default-language:     Haskell98\r\n  Build-Tools:          happy >= 1.19\r\n  Build-Depends:        array >= 0.1, pretty >= 1.0,\r\n                        base >= 4.5 && < 4.13,\r\n                        -- this is needed to access GHC.Generics on GHC 7.4\r\n                        ghc-prim\r\n  -- this is needed to access Data.Semigroup and Control.Monad.Fail on GHCs\r\n  -- before 8.0\r\n  if !impl(ghc >= 8.0)\r\n    Build-Depends:\r\n                        semigroups >= 0.18.3,\r\n                        fail == 4.9.*\r\n\r\n  Exposed-modules:      Language.Haskell.Exts,\r\n                        Language.Haskell.Exts.Lexer,\r\n                        Language.Haskell.Exts.Pretty,\r\n                        Language.Haskell.Exts.Extension,\r\n                        Language.Haskell.Exts.Build,\r\n                        Language.Haskell.Exts.SrcLoc,\r\n\r\n                        Language.Haskell.Exts.Syntax,\r\n                        Language.Haskell.Exts.Fixity,\r\n                        Language.Haskell.Exts.ExactPrint,\r\n                        Language.Haskell.Exts.Parser,\r\n                        Language.Haskell.Exts.Comments\r\n\r\n  Other-modules:        Language.Haskell.Exts.ExtScheme,\r\n                        Language.Haskell.Exts.ParseMonad,\r\n                        Language.Haskell.Exts.ParseSyntax,\r\n                        Language.Haskell.Exts.InternalLexer,\r\n                        Language.Haskell.Exts.ParseUtils,\r\n                        Language.Haskell.Exts.InternalParser\r\n                        Language.Preprocessor.Unlit\r\n  Hs-source-dirs:       src\r\n  Ghc-options:          -Wall\r\n\r\nSource-Repository head\r\n        Type:           git\r\n        Location:       https://github.com/haskell-suite/haskell-src-exts.git\r\n\r\nTest-Suite test\r\n  type:                exitcode-stdio-1.0\r\n  hs-source-dirs:      tests\r\n  main-is:             Runner.hs\r\n  other-modules:       Extensions\r\n  GHC-Options:         -threaded -Wall\r\n  Default-language:    Haskell2010\r\n  Build-depends:       base < 5,\r\n                       mtl,\r\n                       containers,\r\n                       haskell-src-exts,\r\n                       smallcheck >= 1.0,\r\n                       tasty >= 0.3,\r\n                       tasty-smallcheck,\r\n                       tasty-golden >= 2.2.2,\r\n                       filepath,\r\n                       directory,\r\n                       pretty-show >= 1.6.16\r\n";
    }