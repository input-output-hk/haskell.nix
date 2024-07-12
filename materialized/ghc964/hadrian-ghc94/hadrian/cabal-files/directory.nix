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
      identifier = { name = "directory"; version = "1.3.8.1"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "libraries@haskell.org";
      author = "";
      homepage = "";
      url = "";
      synopsis = "Platform-agnostic library for filesystem operations";
      description = "This library provides a basic set of operations for manipulating files and\ndirectories in a portable way.";
      buildType = "Configure";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
        ] ++ (if system.isWindows
          then [ (hsPkgs."Win32" or (errorHandler.buildDepError "Win32")) ]
          else [ (hsPkgs."unix" or (errorHandler.buildDepError "unix")) ]);
        buildable = true;
      };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
          ] ++ (if system.isWindows
            then [ (hsPkgs."Win32" or (errorHandler.buildDepError "Win32")) ]
            else [ (hsPkgs."unix" or (errorHandler.buildDepError "unix")) ]);
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/directory-1.3.8.1.tar.gz";
      sha256 = "bd8253197587d32d4553070d2de89d3817176860932b0e9ab7bb7ba3759d8e9c";
    });
  }) // {
    package-description-override = "cabal-version:  2.2\r\nname:           directory\r\nversion:        1.3.8.1\r\nx-revision: 1\r\nlicense:        BSD-3-Clause\r\nlicense-file:   LICENSE\r\nmaintainer:     libraries@haskell.org\r\nbug-reports:    https://github.com/haskell/directory/issues\r\nsynopsis:       Platform-agnostic library for filesystem operations\r\ndescription:\r\n  This library provides a basic set of operations for manipulating files and\r\n  directories in a portable way.\r\ncategory:       System\r\nbuild-type:     Configure\r\ntested-with:    GHC == 8.6.5 || == 8.10.7 || == 9.0.2 || == 9.2.4 || == 9.4.3\r\n\r\nextra-tmp-files:\r\n    autom4te.cache\r\n    config.log\r\n    config.status\r\n    HsDirectoryConfig.h\r\n\r\nextra-source-files:\r\n    HsDirectoryConfig.h.in\r\n    README.md\r\n    System/Directory/Internal/*.h\r\n    changelog.md\r\n    configure\r\n    configure.ac\r\n    directory.buildinfo\r\n    tests/*.hs\r\n    tests/util.inl\r\n\r\nsource-repository head\r\n    type:     git\r\n    location: https://github.com/haskell/directory\r\n\r\nLibrary\r\n    default-language: Haskell2010\r\n    other-extensions: CApiFFI, CPP\r\n\r\n    exposed-modules:\r\n        System.Directory\r\n        System.Directory.OsPath\r\n        System.Directory.Internal\r\n        System.Directory.Internal.Prelude\r\n    other-modules:\r\n        System.Directory.Internal.C_utimensat\r\n        System.Directory.Internal.Common\r\n        System.Directory.Internal.Config\r\n        System.Directory.Internal.Posix\r\n        System.Directory.Internal.Windows\r\n\r\n    include-dirs: .\r\n\r\n    build-depends:\r\n        base     >= 4.11.0 && < 4.20,\r\n        time     >= 1.8.0 && < 1.13,\r\n        filepath >= 1.4.100 && < 1.5\r\n    if os(windows)\r\n        build-depends: Win32 >= 2.13.3 && < 2.14\r\n    else\r\n        build-depends: unix >= 2.8.0 && < 2.9\r\n\r\n    ghc-options: -Wall\r\n\r\ntest-suite test\r\n    default-language: Haskell2010\r\n    other-extensions: BangPatterns, CPP\r\n    default-extensions: OverloadedStrings\r\n    ghc-options:      -Wall\r\n    hs-source-dirs:   tests\r\n    main-is:          Main.hs\r\n    type:             exitcode-stdio-1.0\r\n    build-depends:    base, directory, filepath, time\r\n    if os(windows)\r\n        build-depends: Win32\r\n    else\r\n        build-depends: unix\r\n    other-modules:\r\n        TestUtils\r\n        Util\r\n        -- test-modules-begin\r\n        CanonicalizePath\r\n        CopyFile001\r\n        CopyFile002\r\n        CopyFileWithMetadata\r\n        CreateDirectory001\r\n        CreateDirectoryIfMissing001\r\n        CurrentDirectory001\r\n        Directory001\r\n        DoesDirectoryExist001\r\n        DoesPathExist\r\n        FileTime\r\n        FindFile001\r\n        GetDirContents001\r\n        GetDirContents002\r\n        GetFileSize\r\n        GetHomeDirectory001\r\n        GetHomeDirectory002\r\n        GetPermissions001\r\n        LongPaths\r\n        MakeAbsolute\r\n        MinimizeNameConflicts\r\n        PathIsSymbolicLink\r\n        RemoveDirectoryRecursive001\r\n        RemovePathForcibly\r\n        RenameDirectory\r\n        RenameFile001\r\n        RenamePath\r\n        Simplify\r\n        T8482\r\n        WithCurrentDirectory\r\n        Xdg\r\n        -- test-modules-end\r\n";
  }