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
      specVersion = "1.8";
      identifier = { name = "random"; version = "1.1"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "carter dot schonwald at google mail dot com";
      author = "";
      homepage = "";
      url = "";
      synopsis = "random number library";
      description = "This package provides a basic random number generation\nlibrary, including the ability to split random number\ngenerators.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          ];
        buildable = true;
        };
      tests = {
        "T7936" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            ];
          buildable = true;
          };
        "TestRandomRs" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            ];
          buildable = true;
          };
        "TestRandomIOs" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/random-1.1.tar.gz";
      sha256 = "b718a41057e25a3a71df693ab0fe2263d492e759679b3c2fea6ea33b171d3a5a";
      });
    }) // {
    package-description-override = "name:\t\trandom\r\nversion:\t1.1\r\nx-revision: 1\r\n\r\n\r\n\r\n\r\nlicense:\tBSD3\r\nlicense-file:\tLICENSE\r\nmaintainer:\tcarter dot schonwald at google mail dot com\r\nbug-reports:\thttps://github.com/haskell/random/issues\r\nsynopsis:\trandom number library\r\ncategory:       System\r\ndescription:\r\n\tThis package provides a basic random number generation\r\n\tlibrary, including the ability to split random number\r\n\tgenerators.\r\n\r\nextra-source-files:\r\n  .travis.yml\r\n  README.md\r\n  CHANGELOG.md\r\n  .gitignore\r\n  .darcs-boring\r\n\r\n\r\n\r\nbuild-type: Simple\r\n-- cabal-version 1.8 needed because \"the field 'build-depends: random' refers\r\n-- to a library which is defined within the same package\"\r\ncabal-version: >= 1.8\r\n\r\n\r\n\r\nLibrary\r\n    exposed-modules:\r\n        System.Random\r\n    extensions:\tCPP\r\n    GHC-Options: -O2\r\n    build-depends: base >= 3 && < 5, time\r\n\r\nsource-repository head\r\n    type:     git\r\n    location: http://git.haskell.org/packages/random.git\r\n\r\n-- To run the Test-Suite:\r\n-- $ cabal configure --enable-tests\r\n-- $ cabal test --show-details=always --test-options=\"+RTS -M1M -RTS\"\r\n\r\nTest-Suite T7936\r\n    type:           exitcode-stdio-1.0\r\n    main-is:        T7936.hs\r\n    hs-source-dirs: tests\r\n    build-depends:  base >= 3 && < 5, random\r\n    ghc-options:    -rtsopts -O2\r\n\r\nTest-Suite TestRandomRs\r\n    type:           exitcode-stdio-1.0\r\n    main-is:        TestRandomRs.hs\r\n    hs-source-dirs: tests\r\n    build-depends:  base >= 3 && < 5, random\r\n    ghc-options:    -rtsopts -O2\r\n    -- TODO. Why does the following not work?\r\n    --test-options:   +RTS -M1M -RTS\r\n\r\nTest-Suite TestRandomIOs\r\n    type:           exitcode-stdio-1.0\r\n    main-is:        TestRandomIOs.hs\r\n    hs-source-dirs: tests\r\n    build-depends:  base >= 3 && < 5, random\r\n    ghc-options:    -rtsopts -O2\r\n";
    }