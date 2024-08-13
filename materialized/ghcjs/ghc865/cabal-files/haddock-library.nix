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
      identifier = { name = "haddock-library"; version = "1.10.0"; };
      license = "BSD-2-Clause";
      copyright = "";
      maintainer = "Alec Theriault <alec.theriault@gmail.com>, Alex Biehl <alexbiehl@gmail.com>, Simon Hengel <sol@typeful.net>, Mateusz Kowalczyk <fuuzetsu@fuuzetsu.co.uk>";
      author = "";
      homepage = "http://www.haskell.org/haddock/";
      url = "";
      synopsis = "Library exposing some functionality of Haddock.";
      description = "Haddock is a documentation-generation tool for Haskell\nlibraries. These modules expose some\nfunctionality of it without pulling in the GHC\ndependency. Please note that the API is likely\nto change so be sure to specify upper bounds in\nyour projects. For interacting with Haddock\nitself, see the [haddock package](https://hackage.haskell.org/package/haddock).";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."parsec" or (errorHandler.buildDepError "parsec"))
          ];
        buildable = true;
        };
      tests = {
        "spec" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."parsec" or (errorHandler.buildDepError "parsec"))
            (hsPkgs."base-compat" or (errorHandler.buildDepError "base-compat"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            ];
          build-tools = [
            (hsPkgs.pkgsBuildBuild.hspec-discover.components.exes.hspec-discover or (pkgs.pkgsBuildBuild.hspec-discover or (errorHandler.buildToolDepError "hspec-discover:hspec-discover")))
            ];
          buildable = true;
          };
        "fixtures" = {
          depends = [
            (hsPkgs."haddock-library" or (errorHandler.buildDepError "haddock-library"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base-compat" or (errorHandler.buildDepError "base-compat"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."tree-diff" or (errorHandler.buildDepError "tree-diff"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/haddock-library-1.10.0.tar.gz";
      sha256 = "f806c7d5a07d63166101332664c345278f88a5781a640ec3ef215c8fb0015395";
      });
    }) // {
    package-description-override = "cabal-version:        2.2\nname:                 haddock-library\nversion:              1.10.0\nsynopsis:             Library exposing some functionality of Haddock.\n\ndescription:          Haddock is a documentation-generation tool for Haskell\n                      libraries. These modules expose some\n                      functionality of it without pulling in the GHC\n                      dependency. Please note that the API is likely\n                      to change so be sure to specify upper bounds in\n                      your projects. For interacting with Haddock\n                      itself, see the [haddock package](https://hackage.haskell.org/package/haddock).\n\nlicense:              BSD-2-Clause\nlicense-file:         LICENSE\nmaintainer:           Alec Theriault <alec.theriault@gmail.com>, Alex Biehl <alexbiehl@gmail.com>, Simon Hengel <sol@typeful.net>, Mateusz Kowalczyk <fuuzetsu@fuuzetsu.co.uk>\nhomepage:             http://www.haskell.org/haddock/\nbug-reports:          https://github.com/haskell/haddock/issues\ncategory:             Documentation\ntested-with:          GHC == 7.4.2\n                    , GHC == 7.6.3\n                    , GHC == 7.8.4\n                    , GHC == 7.10.3\n                    , GHC == 8.0.2\n                    , GHC == 8.2.2\n                    , GHC == 8.4.4\n                    , GHC == 8.6.5\n                    , GHC == 8.8.3\n                    , GHC == 8.10.1\n                    , GHC == 9.0.1\n\nextra-source-files:\n  CHANGES.md\n  fixtures/examples/*.input\n  fixtures/examples/*.parsed\n\ncommon lib-defaults\n  default-language: Haskell2010\n\n  build-depends:\n    , base         >= 4.5     && < 4.16\n    , bytestring   ^>= 0.9.2.1 || ^>= 0.10.0.0\n    , containers   ^>= 0.4.2.1 || ^>= 0.5.0.0 || ^>= 0.6.0.1\n    , transformers ^>= 0.3.0.0 || ^>= 0.4.1.0 || ^>= 0.5.0.0\n    , text         ^>= 1.2.3.0\n    , parsec       ^>= 3.1.13.0\n\n  ghc-options: -funbox-strict-fields -Wall\n  if impl(ghc >= 8.0)\n    ghc-options: -Wcompat -Wnoncanonical-monad-instances\n\nlibrary\n  import: lib-defaults\n\n  hs-source-dirs:       src\n\n  exposed-modules:\n    Documentation.Haddock.Doc\n    Documentation.Haddock.Markup\n    Documentation.Haddock.Parser\n    Documentation.Haddock.Types\n\n  other-modules:\n    CompatPrelude\n    Documentation.Haddock.Parser.Util\n    Documentation.Haddock.Parser.Monad\n    Documentation.Haddock.Parser.Identifier\n\ntest-suite spec\n  import: lib-defaults\n  type:             exitcode-stdio-1.0\n  main-is:          Spec.hs\n  hs-source-dirs:\n    test\n    src\n\n  other-modules:\n    CompatPrelude\n    Documentation.Haddock.Doc\n    Documentation.Haddock.Markup\n    Documentation.Haddock.Parser\n    Documentation.Haddock.Parser.Monad\n    Documentation.Haddock.Parser.Util\n    Documentation.Haddock.Parser.UtilSpec\n    Documentation.Haddock.ParserSpec\n    Documentation.Haddock.Types\n    Documentation.Haddock.Parser.Identifier\n\n  build-depends:\n    , base-compat  ^>= 0.9.3 || ^>= 0.11.0\n    , QuickCheck   ^>= 2.11  || ^>= 2.13.2 || ^>= 2.14 \n    , deepseq      ^>= 1.3.0.0 || ^>= 1.4.0.0\n\n  -- NB: build-depends & build-tool-depends have independent\n  --     install-plans, so it's best to limit to a single major\n  --     version of `hspec` & `hspec-discover` to ensure\n  --     intercompatibility\n  build-depends:\n    , hspec                          >= 2.4.4    && < 2.8\n\n  build-tool-depends:\n    , hspec-discover:hspec-discover  >= 2.4.4    && < 2.8\n\ntest-suite fixtures\n  type:             exitcode-stdio-1.0\n  default-language: Haskell2010\n  main-is:          Fixtures.hs\n  ghc-options:      -Wall\n  hs-source-dirs:   fixtures\n  build-depends:\n      -- intra-package dependency\n    , haddock-library\n      -- constraints inherited via lib:haddock-library component\n    , base\n\n      -- extra dependencies\n    , base-compat           ^>= 0.9.3 || ^>= 0.11.0\n    , directory             ^>= 1.3.0.2\n    , filepath              ^>= 1.4.1.2\n    , optparse-applicative  ^>= 0.15\n    , tree-diff             ^>= 0.1\n\nsource-repository head\n  type:     git\n  subdir:   haddock-library\n  location: https://github.com/haskell/haddock.git\n";
    }