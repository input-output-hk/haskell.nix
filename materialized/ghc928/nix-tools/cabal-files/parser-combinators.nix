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
      specVersion = "1.18";
      identifier = { name = "parser-combinators"; version = "1.3.0"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Mark Karpov <markkarpov92@gmail.com>";
      author = "Mark Karpov <markkarpov92@gmail.com>\nAlex Washburn <github@recursion.ninja>";
      homepage = "https://github.com/mrkkrp/parser-combinators";
      url = "";
      synopsis = "Lightweight package providing commonly useful parser combinators";
      description = "Lightweight package providing commonly useful parser combinators.";
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
      url = "http://hackage.haskell.org/package/parser-combinators-1.3.0.tar.gz";
      sha256 = "9310ef0d49f8a8922acda10b1cded9854cbee04dea717effc6ee5983072e4447";
      });
    }) // {
    package-description-override = "cabal-version:   1.18\nname:            parser-combinators\nversion:         1.3.0\nlicense:         BSD3\nlicense-file:    LICENSE.md\nmaintainer:      Mark Karpov <markkarpov92@gmail.com>\nauthor:\n    Mark Karpov <markkarpov92@gmail.com>\n    Alex Washburn <github@recursion.ninja>\n\ntested-with:     ghc ==8.6.5 ghc ==8.8.4 ghc ==8.10.3\nhomepage:        https://github.com/mrkkrp/parser-combinators\nbug-reports:     https://github.com/mrkkrp/parser-combinators/issues\nsynopsis:\n    Lightweight package providing commonly useful parser combinators\n\ndescription:\n    Lightweight package providing commonly useful parser combinators.\n\ncategory:        Parsing\nbuild-type:      Simple\nextra-doc-files:\n    CHANGELOG.md\n    README.md\n\nsource-repository head\n    type:     git\n    location: https://github.com/mrkkrp/parser-combinators.git\n\nflag dev\n    description: Turn on development settings.\n    default:     False\n    manual:      True\n\nlibrary\n    exposed-modules:\n        Control.Applicative.Combinators\n        Control.Applicative.Combinators.NonEmpty\n        Control.Applicative.Permutations\n        Control.Monad.Combinators\n        Control.Monad.Combinators.Expr\n        Control.Monad.Combinators.NonEmpty\n        Control.Monad.Permutations\n\n    default-language: Haskell2010\n    build-depends:    base >=4.12 && <5.0\n\n    if flag(dev)\n        ghc-options: -Wall -Werror\n\n    else\n        ghc-options: -O2 -Wall\n\n    if flag(dev)\n        ghc-options:\n            -Wcompat -Wincomplete-record-updates -Wincomplete-uni-patterns\n            -Wnoncanonical-monad-instances\n";
    }