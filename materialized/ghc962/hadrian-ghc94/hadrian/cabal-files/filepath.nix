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
    flags = { cpphs = false; };
    package = {
      specVersion = "2.2";
      identifier = { name = "filepath"; version = "1.4.100.3"; };
      license = "BSD-3-Clause";
      copyright = "Neil Mitchell 2005-2020, Julain Ospald 2021-2022";
      maintainer = "Julian Ospald <hasufell@posteo.de>";
      author = "Neil Mitchell <ndmitchell@gmail.com>";
      homepage = "https://github.com/haskell/filepath/blob/master/README.md";
      url = "";
      synopsis = "Library for manipulating FilePaths in a cross platform way.";
      description = "This package provides functionality for manipulating @FilePath@ values, and is shipped with <https://www.haskell.org/ghc/ GHC>. It provides two variants for filepaths:\n\n1. legacy filepaths: @type FilePath = String@\n\n2. operating system abstracted filepaths (@OsPath@): internally unpinned @ShortByteString@ (platform-dependent encoding)\n\nIt is recommended to use @OsPath@ when possible, because it is more correct.\n\nFor each variant there are three main modules:\n\n* \"System.FilePath.Posix\" / \"System.OsPath.Posix\" manipulates POSIX\\/Linux style @FilePath@ values (with @\\/@ as the path separator).\n\n* \"System.FilePath.Windows\" / \"System.OsPath.Windows\" manipulates Windows style @FilePath@ values (with either @\\\\@ or @\\/@ as the path separator, and deals with drives).\n\n* \"System.FilePath\" / \"System.OsPath\" for dealing with current platform-specific filepaths\n\n\"System.OsString\" is like \"System.OsPath\", but more general purpose. Refer to the documentation of\nthose modules for more information.\n\nAn introduction into the new API can be found in this\n<https://hasufell.github.io/posts/2022-06-29-fixing-haskell-filepaths.html blog post>.\nCode examples for the new API can be found <https://github.com/hasufell/filepath-examples here>.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          ];
        build-tools = (pkgs.lib).optional (flags.cpphs) (hsPkgs.buildPackages.cpphs.components.exes.cpphs or (pkgs.buildPackages.cpphs or (errorHandler.buildToolDepError "cpphs:cpphs")));
        buildable = true;
        };
      tests = {
        "filepath-tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            ];
          buildable = true;
          };
        "filepath-equivalent-tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            ];
          buildable = true;
          };
        "bytestring-tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            ];
          buildable = true;
          };
        "abstract-filepath" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."checkers" or (errorHandler.buildDepError "checkers"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            ];
          buildable = true;
          };
        };
      benchmarks = {
        "bench-filepath" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/filepath-1.4.100.3.tar.gz";
      sha256 = "ed1d11173f5052461cd1df58b5ef4abbfa1803ad2b237da7ddb7c7d64e017de2";
      });
    }) // {
    package-description-override = "cabal-version:      2.2\r\nname:               filepath\r\nversion:            1.4.100.3\r\nx-revision: 1\r\n\r\n-- NOTE: Don't forget to update ./changelog.md\r\nlicense:            BSD-3-Clause\r\nlicense-file:       LICENSE\r\nauthor:             Neil Mitchell <ndmitchell@gmail.com>\r\nmaintainer:         Julian Ospald <hasufell@posteo.de>\r\ncopyright:          Neil Mitchell 2005-2020, Julain Ospald 2021-2022\r\nbug-reports:        https://github.com/haskell/filepath/issues\r\nhomepage:\r\n  https://github.com/haskell/filepath/blob/master/README.md\r\n\r\ncategory:           System\r\nbuild-type:         Simple\r\nsynopsis:           Library for manipulating FilePaths in a cross platform way.\r\ntested-with:\r\n  GHC ==8.0.2\r\n   || ==8.2.2\r\n   || ==8.4.4\r\n   || ==8.6.5\r\n   || ==8.8.4\r\n   || ==8.10.7\r\n   || ==9.0.2\r\n   || ==9.2.3\r\n\r\ndescription:\r\n  This package provides functionality for manipulating @FilePath@ values, and is shipped with <https://www.haskell.org/ghc/ GHC>. It provides two variants for filepaths:\r\n  .\r\n  1. legacy filepaths: @type FilePath = String@\r\n  .\r\n  2. operating system abstracted filepaths (@OsPath@): internally unpinned @ShortByteString@ (platform-dependent encoding)\r\n  .\r\n  It is recommended to use @OsPath@ when possible, because it is more correct.\r\n  .\r\n  For each variant there are three main modules:\r\n  .\r\n  * \"System.FilePath.Posix\" / \"System.OsPath.Posix\" manipulates POSIX\\/Linux style @FilePath@ values (with @\\/@ as the path separator).\r\n  .\r\n  * \"System.FilePath.Windows\" / \"System.OsPath.Windows\" manipulates Windows style @FilePath@ values (with either @\\\\@ or @\\/@ as the path separator, and deals with drives).\r\n  .\r\n  * \"System.FilePath\" / \"System.OsPath\" for dealing with current platform-specific filepaths\r\n  .\r\n  \"System.OsString\" is like \"System.OsPath\", but more general purpose. Refer to the documentation of\r\n  those modules for more information.\r\n  .\r\n  An introduction into the new API can be found in this\r\n  <https://hasufell.github.io/posts/2022-06-29-fixing-haskell-filepaths.html blog post>.\r\n  Code examples for the new API can be found <https://github.com/hasufell/filepath-examples here>.\r\n\r\nextra-source-files:\r\n  Generate.hs\r\n  Makefile\r\n  System/FilePath/Internal.hs\r\n  System/OsPath/Common.hs\r\n  System/OsString/Common.hs\r\n  tests/bytestring-tests/Properties/Common.hs\r\n\r\nextra-doc-files:\r\n  changelog.md\r\n  HACKING.md\r\n  README.md\r\n\r\nflag cpphs\r\n  description: Use cpphs (fixes haddock source links)\r\n  default:     False\r\n  manual:      True\r\n\r\nsource-repository head\r\n  type:     git\r\n  location: https://github.com/haskell/filepath\r\n\r\nlibrary\r\n  exposed-modules:\r\n    System.FilePath\r\n    System.FilePath.Posix\r\n    System.FilePath.Windows\r\n    System.OsPath\r\n    System.OsPath.Data.ByteString.Short\r\n    System.OsPath.Data.ByteString.Short.Internal\r\n    System.OsPath.Data.ByteString.Short.Word16\r\n    System.OsPath.Encoding\r\n    System.OsPath.Encoding.Internal\r\n    System.OsPath.Internal\r\n    System.OsPath.Posix\r\n    System.OsPath.Posix.Internal\r\n    System.OsPath.Types\r\n    System.OsPath.Windows\r\n    System.OsPath.Windows.Internal\r\n    System.OsString\r\n    System.OsString.Internal\r\n    System.OsString.Internal.Types\r\n    System.OsString.Posix\r\n    System.OsString.Windows\r\n\r\n  other-extensions:\r\n    CPP\r\n    PatternGuards\r\n\r\n  if impl(ghc >=7.2)\r\n    other-extensions: Safe\r\n\r\n  default-language: Haskell2010\r\n  build-depends:\r\n    , base              >=4.9      && <4.20\r\n    , bytestring        >=0.11.3.0\r\n    , deepseq\r\n    , exceptions\r\n    , template-haskell\r\n\r\n  ghc-options:      -Wall\r\n\r\n  if flag(cpphs)\r\n    ghc-options:        -pgmPcpphs -optP--cpp\r\n    build-tool-depends: cpphs:cpphs -any\r\n\r\ntest-suite filepath-tests\r\n  type:             exitcode-stdio-1.0\r\n  main-is:          Test.hs\r\n  hs-source-dirs:   tests tests/filepath-tests\r\n  other-modules:\r\n    TestGen\r\n    TestUtil\r\n\r\n  build-depends:\r\n    , base\r\n    , bytestring  >=0.11.3.0\r\n    , filepath\r\n    , QuickCheck  >=2.7      && <2.15\r\n\r\n  default-language: Haskell2010\r\n  ghc-options:      -Wall\r\n\r\ntest-suite filepath-equivalent-tests\r\n  default-language: Haskell2010\r\n  ghc-options:      -Wall\r\n  type:             exitcode-stdio-1.0\r\n  main-is:          TestEquiv.hs\r\n  hs-source-dirs:   tests tests/filepath-equivalent-tests\r\n  other-modules:\r\n    Legacy.System.FilePath\r\n    Legacy.System.FilePath.Posix\r\n    Legacy.System.FilePath.Windows\r\n    TestUtil\r\n\r\n  build-depends:\r\n    , base\r\n    , bytestring  >=0.11.3.0\r\n    , filepath\r\n    , QuickCheck  >=2.7      && <2.15\r\n\r\ntest-suite bytestring-tests\r\n  default-language: Haskell2010\r\n  ghc-options:      -Wall\r\n  type:             exitcode-stdio-1.0\r\n  main-is:          Main.hs\r\n  hs-source-dirs:   tests tests/bytestring-tests\r\n  other-modules:\r\n    Properties.ShortByteString\r\n    Properties.ShortByteString.Word16\r\n    TestUtil\r\n\r\n  build-depends:\r\n    , base\r\n    , bytestring  >=0.11.3.0\r\n    , filepath\r\n    , QuickCheck  >=2.7      && <2.15\r\n\r\ntest-suite abstract-filepath\r\n  default-language: Haskell2010\r\n  ghc-options:      -Wall\r\n  type:             exitcode-stdio-1.0\r\n  main-is:          Test.hs\r\n  hs-source-dirs:   tests tests/abstract-filepath\r\n  other-modules:\r\n    Arbitrary\r\n    EncodingSpec\r\n    OsPathSpec\r\n    TestUtil\r\n\r\n  build-depends:\r\n    , base\r\n    , bytestring  >=0.11.3.0\r\n    , checkers    ^>=0.5.6\r\n    , deepseq\r\n    , filepath\r\n    , QuickCheck  >=2.7      && <2.15\r\n\r\nbenchmark bench-filepath\r\n  default-language: Haskell2010\r\n  ghc-options:      -Wall\r\n  type:             exitcode-stdio-1.0\r\n  main-is:          BenchFilePath.hs\r\n  hs-source-dirs:   bench\r\n  other-modules:    TastyBench\r\n  build-depends:\r\n    , base\r\n    , bytestring  >=0.11.3.0\r\n    , deepseq\r\n    , filepath\r\n\r\n  if impl(ghc >=8.10)\r\n    ghc-options: \"-with-rtsopts=-A32m --nonmoving-gc\"\r\n\r\n  else\r\n    ghc-options: -with-rtsopts=-A32m\r\n";
    }