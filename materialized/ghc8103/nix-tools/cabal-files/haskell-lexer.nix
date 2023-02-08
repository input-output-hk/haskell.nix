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
      specVersion = "1.6";
      identifier = { name = "haskell-lexer"; version = "1.1"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "diatchki@galois.com";
      author = "Thomas Hallgren";
      homepage = "https://github.com/yav/haskell-lexer";
      url = "";
      synopsis = "A fully compliant Haskell 98 lexer.";
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
      url = "http://hackage.haskell.org/package/haskell-lexer-1.1.tar.gz";
      sha256 = "313a15cc643322c8badd148867ce25ca1ffc191df9e7eeec5b10bc08c4b563d5";
      });
    }) // {
    package-description-override = "Name:           haskell-lexer\nVersion:        1.1\nLicense:        BSD3\nLicense-file:   LICENSE\nAuthor:         Thomas Hallgren\nMaintainer:     diatchki@galois.com\nCategory:       Language\nSynopsis:       A fully compliant Haskell 98 lexer.\nDescription:    A fully compliant Haskell 98 lexer.\nBuild-type:     Simple\nCabal-version:  >= 1.6\n\nHomepage:            https://github.com/yav/haskell-lexer\nBug-reports:         https://github.com/yav/haskell-lexer/issues\n\nLibrary\n  Build-Depends:  base < 5\n  Exposed-modules:  Language.Haskell.Lexer\n  Other-modules:    Language.Haskell.Lexer.Layout,\n                    Language.Haskell.Lexer.Tokens,\n                    Language.Haskell.Lexer.Lex,\n                    Language.Haskell.Lexer.Utils,\n                    Language.Haskell.Lexer.Position\n\n  GHC-options:    -O2 -Wall\n\nsource-repository head\n  type:     git\n  location: https://github.com/yav/haskell-lexer.git\n";
    }