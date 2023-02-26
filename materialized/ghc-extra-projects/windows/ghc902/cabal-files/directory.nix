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
      identifier = { name = "directory"; version = "1.3.7.0"; };
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
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/directory-1.3.7.0.tar.gz";
      sha256 = "9f86bd60a4909eef66907c15fc108883439b8fba3b428fb56aaa056631b62d10";
      });
    }) // {
    package-description-override = "name:           directory\r\nversion:        1.3.7.0\r\nx-revision: 2\r\nlicense:        BSD3\r\nlicense-file:   LICENSE\r\nmaintainer:     libraries@haskell.org\r\nbug-reports:    https://github.com/haskell/directory/issues\r\nsynopsis:       Platform-agnostic library for filesystem operations\r\ndescription:\r\n  This library provides a basic set of operations for manipulating files and\r\n  directories in a portable way.\r\ncategory:       System\r\nbuild-type:     Configure\r\ncabal-version:  >= 1.10\r\ntested-with:    GHC>=7.4.1\r\n\r\nextra-tmp-files:\r\n    autom4te.cache\r\n    config.log\r\n    config.status\r\n    HsDirectoryConfig.h\r\n\r\nextra-source-files:\r\n    HsDirectoryConfig.h.in\r\n    README.md\r\n    System/Directory/Internal/*.h\r\n    changelog.md\r\n    configure\r\n    configure.ac\r\n    directory.buildinfo\r\n    tests/*.hs\r\n    tests/util.inl\r\n\r\nsource-repository head\r\n    type:     git\r\n    location: https://github.com/haskell/directory\r\n\r\nLibrary\r\n    default-language: Haskell2010\r\n    other-extensions:\r\n        CPP\r\n        Trustworthy\r\n\r\n    exposed-modules:\r\n        System.Directory\r\n        System.Directory.Internal\r\n        System.Directory.Internal.Prelude\r\n    other-modules:\r\n        System.Directory.Internal.C_utimensat\r\n        System.Directory.Internal.Common\r\n        System.Directory.Internal.Config\r\n        System.Directory.Internal.Posix\r\n        System.Directory.Internal.Windows\r\n\r\n    include-dirs: .\r\n\r\n    build-depends:\r\n        base     >= 4.5 && < 4.17,\r\n        time     >= 1.4 && < 1.13,\r\n        filepath >= 1.3 && < 1.5\r\n    if os(windows)\r\n        build-depends: Win32 >= 2.2.2 && < 2.14\r\n    else\r\n        build-depends: unix >= 2.5.1 && < 2.9\r\n\r\n    ghc-options: -Wall\r\n\r\ntest-suite test\r\n    default-language: Haskell2010\r\n    other-extensions: BangPatterns, CPP, Safe\r\n    ghc-options:      -Wall\r\n    hs-source-dirs:   tests\r\n    main-is:          Main.hs\r\n    type:             exitcode-stdio-1.0\r\n    build-depends:    base, directory, filepath, time\r\n    if os(windows)\r\n        build-depends: Win32\r\n    else\r\n        build-depends: unix\r\n    other-modules:\r\n        TestUtils\r\n        Util\r\n        -- test-modules-begin\r\n        CanonicalizePath\r\n        CopyFile001\r\n        CopyFile002\r\n        CopyFileWithMetadata\r\n        CreateDirectory001\r\n        CreateDirectoryIfMissing001\r\n        CurrentDirectory001\r\n        Directory001\r\n        DoesDirectoryExist001\r\n        DoesPathExist\r\n        FileTime\r\n        FindFile001\r\n        GetDirContents001\r\n        GetDirContents002\r\n        GetFileSize\r\n        GetHomeDirectory001\r\n        GetHomeDirectory002\r\n        GetPermissions001\r\n        LongPaths\r\n        MakeAbsolute\r\n        PathIsSymbolicLink\r\n        RemoveDirectoryRecursive001\r\n        RemovePathForcibly\r\n        RenameDirectory\r\n        RenameFile001\r\n        RenamePath\r\n        Safe\r\n        Simplify\r\n        T8482\r\n        WithCurrentDirectory\r\n        Xdg\r\n        -- test-modules-end\r\n";
    }