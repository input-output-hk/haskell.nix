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
      identifier = { name = "directory"; version = "1.3.8.0"; };
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
      url = "http://hackage.haskell.org/package/directory-1.3.8.0.tar.gz";
      sha256 = "dbf7bb2d10e524c43f799a3e75a2cd069e71359facb875f4dc4052bde2c1bd37";
      });
    }) // {
    package-description-override = "name:           directory\nversion:        1.3.8.0\nlicense:        BSD3\nlicense-file:   LICENSE\nmaintainer:     libraries@haskell.org\nbug-reports:    https://github.com/haskell/directory/issues\nsynopsis:       Platform-agnostic library for filesystem operations\ndescription:\n  This library provides a basic set of operations for manipulating files and\n  directories in a portable way.\ncategory:       System\nbuild-type:     Configure\ncabal-version:  >= 1.10\ntested-with:    GHC>=7.4.1\n\nextra-tmp-files:\n    autom4te.cache\n    config.log\n    config.status\n    HsDirectoryConfig.h\n\nextra-source-files:\n    HsDirectoryConfig.h.in\n    README.md\n    System/Directory/Internal/*.h\n    changelog.md\n    configure\n    configure.ac\n    directory.buildinfo\n    tests/*.hs\n    tests/util.inl\n\nsource-repository head\n    type:     git\n    location: https://github.com/haskell/directory\n\nLibrary\n    default-language: Haskell2010\n    other-extensions: CPP\n\n    exposed-modules:\n        System.Directory\n        System.Directory.OsPath\n        System.Directory.Internal\n        System.Directory.Internal.Prelude\n    other-modules:\n        System.Directory.Internal.C_utimensat\n        System.Directory.Internal.Common\n        System.Directory.Internal.Config\n        System.Directory.Internal.Posix\n        System.Directory.Internal.Windows\n\n    include-dirs: .\n\n    build-depends:\n        base     >= 4.11.0 && < 4.18,\n        time     >= 1.8.0 && < 1.13,\n        filepath >= 1.4.100 && < 1.5\n    if os(windows)\n        build-depends: Win32 >= 2.13.3 && < 2.14\n    else\n        build-depends: unix >= 2.8.0 && < 2.9\n\n    ghc-options: -Wall\n\ntest-suite test\n    default-language: Haskell2010\n    other-extensions: BangPatterns, CPP\n    default-extensions: OverloadedStrings\n    ghc-options:      -Wall\n    hs-source-dirs:   tests\n    main-is:          Main.hs\n    type:             exitcode-stdio-1.0\n    build-depends:    base, directory, filepath, time\n    if os(windows)\n        build-depends: Win32\n    else\n        build-depends: unix\n    other-modules:\n        TestUtils\n        Util\n        -- test-modules-begin\n        CanonicalizePath\n        CopyFile001\n        CopyFile002\n        CopyFileWithMetadata\n        CreateDirectory001\n        CreateDirectoryIfMissing001\n        CurrentDirectory001\n        Directory001\n        DoesDirectoryExist001\n        DoesPathExist\n        FileTime\n        FindFile001\n        GetDirContents001\n        GetDirContents002\n        GetFileSize\n        GetHomeDirectory001\n        GetHomeDirectory002\n        GetPermissions001\n        LongPaths\n        MakeAbsolute\n        MinimizeNameConflicts\n        PathIsSymbolicLink\n        RemoveDirectoryRecursive001\n        RemovePathForcibly\n        RenameDirectory\n        RenameFile001\n        RenamePath\n        Simplify\n        T8482\n        WithCurrentDirectory\n        Xdg\n        -- test-modules-end\n";
    }