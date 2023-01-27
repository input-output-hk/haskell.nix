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
      identifier = { name = "th-expand-syns"; version = "0.4.7.0"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "haskell.5wlh@gishpuppy.com";
      author = "Daniel Schüssler";
      homepage = "https://github.com/DanielSchuessler/th-expand-syns";
      url = "";
      synopsis = "Expands type synonyms in Template Haskell ASTs";
      description = "Expands type synonyms in Template Haskell ASTs.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."syb" or (errorHandler.buildDepError "syb"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          ];
        buildable = true;
        };
      tests = {
        "test-th-expand-syns" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."th-expand-syns" or (errorHandler.buildDepError "th-expand-syns"))
            (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/th-expand-syns-0.4.7.0.tar.gz";
      sha256 = "a46c3e13988a879ff8a30f29f7d945ef765244ffb50373da95200eff6e42abbe";
      });
    }) // {
    package-description-override = "name:                th-expand-syns\r\nversion:             0.4.7.0\r\nx-revision: 1\r\nsynopsis:            Expands type synonyms in Template Haskell ASTs\r\ndescription:         Expands type synonyms in Template Haskell ASTs.\r\ncategory:            Template Haskell\r\nlicense:             BSD3\r\nlicense-file:        LICENSE\r\nauthor:              Daniel Schüssler\r\nmaintainer:          haskell.5wlh@gishpuppy.com\r\ncabal-version:       >= 1.10\r\nbuild-type:          Simple\r\nextra-source-files:  changelog.markdown\r\nhomepage:            https://github.com/DanielSchuessler/th-expand-syns\r\ntested-with:\r\n    GHC == 8.0.2\r\n    GHC == 8.2.2\r\n    GHC == 8.4.4\r\n    GHC == 8.6.5\r\n    GHC == 8.8.1\r\n\r\nsource-repository head\r\n type: git\r\n location: git://github.com/DanielSchuessler/th-expand-syns.git\r\n\r\nLibrary\r\n    build-depends:       base >= 4 && < 5, template-haskell < 2.18, syb, containers\r\n    ghc-options:\r\n    exposed-modules:     Language.Haskell.TH.ExpandSyns\r\n    other-modules:       Language.Haskell.TH.ExpandSyns.SemigroupCompat\r\n    default-language:    Haskell2010\r\n\r\nTest-Suite test-th-expand-syns\r\n    type:               exitcode-stdio-1.0\r\n    hs-source-dirs:     testing\r\n    main-is:            Main.hs\r\n    other-modules:      Util, Types\r\n    build-depends:      base, th-expand-syns, template-haskell < 2.17\r\n    default-language:   Haskell2010\r\n";
    }