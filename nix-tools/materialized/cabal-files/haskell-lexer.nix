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
      specVersion = "2.2";
      identifier = { name = "haskell-lexer"; version = "1.1.1"; };
      license = "MIT";
      copyright = "";
      maintainer = "diatchki@galois.com";
      author = "Thomas Hallgren";
      homepage = "https://github.com/yav/haskell-lexer";
      url = "";
      synopsis = "A fully compliant Haskell 98 lexer";
      description = "A fully compliant Haskell 98 lexer.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [ (hsPkgs."base" or (errorHandler.buildDepError "base")) ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/haskell-lexer-1.1.1.tar.gz";
      sha256 = "c8eeda569a30f115244c17920247ba2c45d11b3e30805f648a878f7855d8f349";
      });
    }) // {
    package-description-override = "Cabal-version:  2.2\nName:           haskell-lexer\nVersion:        1.1.1\nLicense:        MIT\nLicense-file:   LICENSE\nAuthor:         Thomas Hallgren\nMaintainer:     diatchki@galois.com\nCategory:       Language\nSynopsis:       A fully compliant Haskell 98 lexer\nDescription:    A fully compliant Haskell 98 lexer.\nBuild-type:     Simple\n\nHomepage:            https://github.com/yav/haskell-lexer\nBug-reports:         https://github.com/yav/haskell-lexer/issues\n\ntested-with:\n  GHC == 9.4.1\n  GHC == 9.2.3\n  GHC == 9.0.2\n  GHC == 8.10.7\n  GHC == 8.8.4\n  GHC == 8.6.5\n  GHC == 8.4.4\n  GHC == 8.2.2\n  GHC == 8.0.2\n  GHC == 7.10.3\n  GHC == 7.8.4\n  GHC == 7.6.3\n  GHC == 7.4.2\n  GHC == 7.2.2\n  GHC == 7.0.4\n\nLibrary\n  Build-Depends:  base < 5\n  Exposed-modules:  Language.Haskell.Lexer\n  Other-modules:    Language.Haskell.Lexer.Layout,\n                    Language.Haskell.Lexer.Tokens,\n                    Language.Haskell.Lexer.Lex,\n                    Language.Haskell.Lexer.Utils,\n                    Language.Haskell.Lexer.Position\n\n  GHC-options:\n    -Wall\n    -fno-warn-unused-matches\n    -fno-warn-name-shadowing\n\n  if impl(GHC >= 8.0)\n    GHC-options:\n      -Wcompat\n\n  default-language: Haskell2010\n\nsource-repository head\n  type:     git\n  location: https://github.com/yav/haskell-lexer.git\n";
    }