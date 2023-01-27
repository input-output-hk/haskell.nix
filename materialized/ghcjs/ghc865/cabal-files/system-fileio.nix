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
      identifier = { name = "system-fileio"; version = "0.3.16.4"; };
      license = "MIT";
      copyright = "";
      maintainer = "FP Complete <michael@fpcomplete.com>";
      author = "John Millikin <jmillikin@gmail.com>";
      homepage = "https://github.com/fpco/haskell-filesystem";
      url = "";
      synopsis = "Consistent filesystem interaction across GHC versions (deprecated)";
      description = "Please see: https://plus.google.com/+MichaelSnoyman/posts/Ft5hnPqpgEx";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."system-filepath" or (errorHandler.buildDepError "system-filepath"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          ] ++ (if system.isWindows
          then [
            (hsPkgs."Win32" or (errorHandler.buildDepError "Win32"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            ]
          else [ (hsPkgs."unix" or (errorHandler.buildDepError "unix")) ]);
        buildable = true;
        };
      tests = {
        "filesystem_tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."chell" or (errorHandler.buildDepError "chell"))
            (hsPkgs."system-fileio" or (errorHandler.buildDepError "system-fileio"))
            (hsPkgs."system-filepath" or (errorHandler.buildDepError "system-filepath"))
            (hsPkgs."temporary" or (errorHandler.buildDepError "temporary"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            ] ++ (pkgs.lib).optional (!system.isWindows) (hsPkgs."unix" or (errorHandler.buildDepError "unix"));
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/system-fileio-0.3.16.4.tar.gz";
      sha256 = "34e58b88a19a69ff1a559e211af6edb596e33ee1b1d5f44490febf325c78c6c7";
      });
    }) // {
    package-description-override = "name: system-fileio\nversion: 0.3.16.4\nlicense: MIT\nlicense-file: license.txt\nauthor: John Millikin <jmillikin@gmail.com>\nmaintainer: FP Complete <michael@fpcomplete.com>\nbuild-type: Simple\ncabal-version: >= 1.8\ncategory: System\nstability: experimental\nhomepage: https://github.com/fpco/haskell-filesystem\nbug-reports: https://github.com/fpco/haskell-filesystem/issues\n\nsynopsis: Consistent filesystem interaction across GHC versions (deprecated)\ndescription: Please see: https://plus.google.com/+MichaelSnoyman/posts/Ft5hnPqpgEx\n\nextra-source-files:\n  README.md\n  ChangeLog.md\n  lib/hssystemfileio-unix.h\n  lib/hssystemfileio-win32.h\n  --\n  tests/system-fileio-tests.cabal\n  tests/FilesystemTests.hs\n  tests/FilesystemTests/Posix.hs\n  tests/FilesystemTests/Util.hs\n  tests/FilesystemTests/Windows.hs\n\nsource-repository head\n  type: git\n  location: https://github.com/fpco/haskell-filesystem.git\n\nlibrary\n  ghc-options: -Wall -O2\n  hs-source-dirs: lib\n\n  build-depends:\n      base >= 4.0 && < 5.0\n    , bytestring >= 0.9\n    , system-filepath >= 0.3.1 && < 0.5\n    , text >= 0.7.1\n    , time >= 1.0 && < 2.0\n\n  if os(windows)\n    cpp-options: -DCABAL_OS_WINDOWS\n    build-depends:\n        Win32 >= 2.2\n      , directory >= 1.0\n    c-sources: lib/hssystemfileio-win32.c\n  else\n    build-depends:\n        unix >= 2.3\n    c-sources: lib/hssystemfileio-unix.c\n    if impl(ghc >= 7.2.0) && impl(ghc < 7.4.0)\n      cpp-options: -DSYSTEMFILEIO_LOCAL_OPEN_FILE\n\n  exposed-modules:\n    Filesystem\n\ntest-suite filesystem_tests\n  type: exitcode-stdio-1.0\n  main-is: FilesystemTests.hs\n\n  ghc-options: -Wall -O2\n  cc-options: -Wall\n  hs-source-dirs: tests\n\n  build-depends:\n      base >= 4.0 && < 5.0\n    , bytestring >= 0.9\n    , chell >= 0.4 && < 0.5\n    , system-fileio\n    , system-filepath\n    , temporary >= 1.1 && < 2.0\n    , text\n    , time >= 1.0 && < 2.0\n    , transformers >= 0.2\n\n  if os(windows)\n    cpp-options: -DCABAL_OS_WINDOWS\n  else\n    build-depends:\n        unix >= 2.3\n\n  if os(darwin)\n    cpp-options: -DCABAL_OS_DARWIN\n\n  other-modules:\n    FilesystemTests.Posix\n    FilesystemTests.Util\n    FilesystemTests.Windows\n";
    }