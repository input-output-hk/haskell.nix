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
      identifier = { name = "extra"; version = "1.7.16"; };
      license = "BSD-3-Clause";
      copyright = "Neil Mitchell 2014-2024";
      maintainer = "Neil Mitchell <ndmitchell@gmail.com>";
      author = "Neil Mitchell <ndmitchell@gmail.com>";
      homepage = "https://github.com/ndmitchell/extra#readme";
      url = "";
      synopsis = "Extra functions I use.";
      description = "A library of extra functions for the standard Haskell libraries. Most functions are simple additions, filling out missing functionality. A few functions are available in later versions of GHC, but this package makes them available back to GHC 7.2.\n\nThe module \"Extra\" documents all functions provided by this library. Modules such as \"Data.List.Extra\" provide extra functions over \"Data.List\" and also reexport \"Data.List\". Users are recommended to replace \"Data.List\" imports with \"Data.List.Extra\" if they need the extra functionality.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."process" or (errorHandler.buildDepError "process"))
          (hsPkgs."clock" or (errorHandler.buildDepError "clock"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
        ] ++ pkgs.lib.optional (!system.isWindows) (hsPkgs."unix" or (errorHandler.buildDepError "unix"));
        buildable = true;
      };
      tests = {
        "extra-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."quickcheck-instances" or (errorHandler.buildDepError "quickcheck-instances"))
          ] ++ pkgs.lib.optional (!system.isWindows) (hsPkgs."unix" or (errorHandler.buildDepError "unix"));
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/extra-1.7.16.tar.gz";
      sha256 = "250c6d43c30b2c71f2cf498a10e69e43ac035974d3819529385d99e42ce77c70";
    });
  }) // {
    package-description-override = "cabal-version:      1.18\nbuild-type:         Simple\nname:               extra\nversion:            1.7.16\nlicense:            BSD3\nlicense-file:       LICENSE\ncategory:           Development\nauthor:             Neil Mitchell <ndmitchell@gmail.com>\nmaintainer:         Neil Mitchell <ndmitchell@gmail.com>\ncopyright:          Neil Mitchell 2014-2024\nsynopsis:           Extra functions I use.\ndescription:\n    A library of extra functions for the standard Haskell libraries. Most functions are simple additions, filling out missing functionality. A few functions are available in later versions of GHC, but this package makes them available back to GHC 7.2.\n    .\n    The module \"Extra\" documents all functions provided by this library. Modules such as \"Data.List.Extra\" provide extra functions over \"Data.List\" and also reexport \"Data.List\". Users are recommended to replace \"Data.List\" imports with \"Data.List.Extra\" if they need the extra functionality.\nhomepage:           https://github.com/ndmitchell/extra#readme\nbug-reports:        https://github.com/ndmitchell/extra/issues\ntested-with:        GHC==9.8, GHC==9.6, GHC==9.4, GHC==9.2, GHC==9.0, GHC==8.10, GHC==8.8\n\nextra-doc-files:\n    CHANGES.txt\n    README.md\nextra-source-files:\n    Generate.hs\n\nsource-repository head\n    type:     git\n    location: https://github.com/ndmitchell/extra.git\n\nlibrary\n    default-language: Haskell2010\n    hs-source-dirs: src\n    build-depends:\n        base >= 4.9 && < 5,\n        directory,\n        filepath,\n        process,\n        clock >= 0.7,\n        time\n    if !os(windows)\n        build-depends: unix\n\n    other-modules:\n        Partial\n    exposed-modules:\n        Extra\n        Control.Concurrent.Extra\n        Control.Exception.Extra\n        Control.Monad.Extra\n        Data.Foldable.Extra\n        Data.Either.Extra\n        Data.IORef.Extra\n        Data.List.Extra\n        Data.List.NonEmpty.Extra\n        Data.Monoid.Extra\n        Data.Tuple.Extra\n        Data.Typeable.Extra\n        Data.Version.Extra\n        Numeric.Extra\n        System.Directory.Extra\n        System.Environment.Extra\n        System.Info.Extra\n        System.IO.Extra\n        System.Process.Extra\n        System.Time.Extra\n        Text.Read.Extra\n\ntest-suite extra-test\n    type:            exitcode-stdio-1.0\n    default-language: Haskell2010\n    build-depends:\n        base == 4.*,\n        directory,\n        filepath,\n        extra,\n        QuickCheck >= 2.10,\n        quickcheck-instances >= 0.3.17\n    if !os(windows)\n        build-depends: unix\n    hs-source-dirs: test\n    ghc-options: -main-is Test -threaded \"-with-rtsopts=-N4 -K1K\"\n    main-is:        Test.hs\n    other-modules:\n        TestCustom\n        TestGen\n        TestUtil\n";
  }