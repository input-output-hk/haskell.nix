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
      specVersion = "1.18";
      identifier = { name = "filepattern"; version = "0.1.3"; };
      license = "BSD-3-Clause";
      copyright = "Neil Mitchell 2011-2022";
      maintainer = "Neil Mitchell <ndmitchell@gmail.com>";
      author = "Neil Mitchell <ndmitchell@gmail.com>, Evan Rutledge Borden <evan@evan-borden.com>";
      homepage = "https://github.com/ndmitchell/filepattern#readme";
      url = "";
      synopsis = "File path glob-like matching";
      description = "A library for matching files using patterns such as @\\\"src\\/**\\/*.png\\\"@ for all @.png@ files\nrecursively under the @src@ directory. Features:\n\n* All matching is /O(n)/. Most functions precompute some information given only one argument.\n\n* See \"System.FilePattern\" and @?==@ simple matching and semantics.\n\n* Use @match@ and @substitute@ to extract suitable\nstrings from the @*@ and @**@ matches, and substitute them back into other patterns.\n\n* Use @step@ and @matchMany@ to perform bulk matching\nof many patterns against many paths simultaneously.\n\n* Use \"System.FilePattern.Directory\" to perform optimised directory traverals using patterns.\n\nOriginally taken from the <https://hackage.haskell.org/package/shake Shake library>.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          ];
        buildable = true;
        };
      tests = {
        "filepattern-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
            (hsPkgs."filepattern" or (errorHandler.buildDepError "filepattern"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/filepattern-0.1.3.tar.gz";
      sha256 = "cc445d439ea2f65cac7604d3578aa2c3a62e5a91dc989f4ce5b3390db9e59636";
      });
    }) // {
    package-description-override = "cabal-version:      1.18\nbuild-type:         Simple\nname:               filepattern\nversion:            0.1.3\nlicense:            BSD3\nlicense-file:       LICENSE\ncategory:           Development, FilePath\nauthor:             Neil Mitchell <ndmitchell@gmail.com>, Evan Rutledge Borden <evan@evan-borden.com>\nmaintainer:         Neil Mitchell <ndmitchell@gmail.com>\ncopyright:          Neil Mitchell 2011-2022\nsynopsis:           File path glob-like matching\ndescription:\n    A library for matching files using patterns such as @\\\"src\\/**\\/*.png\\\"@ for all @.png@ files\n    recursively under the @src@ directory. Features:\n    .\n    * All matching is /O(n)/. Most functions precompute some information given only one argument.\n    .\n    * See \"System.FilePattern\" and @?==@ simple matching and semantics.\n    .\n    * Use @match@ and @substitute@ to extract suitable\n    strings from the @*@ and @**@ matches, and substitute them back into other patterns.\n    .\n    * Use @step@ and @matchMany@ to perform bulk matching\n    of many patterns against many paths simultaneously.\n    .\n    * Use \"System.FilePattern.Directory\" to perform optimised directory traverals using patterns.\n    .\n    Originally taken from the <https://hackage.haskell.org/package/shake Shake library>.\nhomepage:           https://github.com/ndmitchell/filepattern#readme\nbug-reports:        https://github.com/ndmitchell/filepattern/issues\ntested-with:        GHC==9.0, GHC==8.10, GHC==8.8, GHC==8.6, GHC==8.4, GHC==8.2, GHC==8.0\nextra-doc-files:\n    CHANGES.txt\n    README.md\n\nsource-repository head\n    type:     git\n    location: https://github.com/ndmitchell/filepattern.git\n\nlibrary\n    default-language: Haskell2010\n    hs-source-dirs: src\n    build-depends:\n        base == 4.*,\n        directory,\n        extra >= 1.6.2,\n        filepath\n    exposed-modules:\n        System.FilePattern\n        System.FilePattern.Directory\n    other-modules:\n        System.FilePattern.Core\n        System.FilePattern.ListBy\n        System.FilePattern.Monads\n        System.FilePattern.Step\n        System.FilePattern.Tree\n        System.FilePattern.Wildcard\n\n\ntest-suite filepattern-test\n    default-language: Haskell2010\n    type: exitcode-stdio-1.0\n    main-is: Test.hs\n    hs-source-dirs: test\n    build-depends:\n        base == 4.*,\n        directory,\n        extra,\n        filepattern,\n        filepath,\n        QuickCheck >= 2.0\n    other-modules:\n        Test.Cases\n        Test.Util\n";
    }