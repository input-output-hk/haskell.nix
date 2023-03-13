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
      identifier = { name = "filepath"; version = "1.4.100.1"; };
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
      url = "http://hackage.haskell.org/package/filepath-1.4.100.1.tar.gz";
      sha256 = "bc14a7fed5365f39ed58bacb870da0f18d3b858100e9cf2d9f4f6a16de026a44";
      });
    }) // {
    package-description-override = "cabal-version:      2.2\nname:               filepath\nversion:            1.4.100.1\n\n-- NOTE: Don't forget to update ./changelog.md\nlicense:            BSD-3-Clause\nlicense-file:       LICENSE\nauthor:             Neil Mitchell <ndmitchell@gmail.com>\nmaintainer:         Julian Ospald <hasufell@posteo.de>\ncopyright:          Neil Mitchell 2005-2020, Julain Ospald 2021-2022\nbug-reports:        https://github.com/haskell/filepath/issues\nhomepage:\n  https://github.com/haskell/filepath/blob/master/README.md\n\ncategory:           System\nbuild-type:         Simple\nsynopsis:           Library for manipulating FilePaths in a cross platform way.\ntested-with:\n  GHC ==8.0.2\n   || ==8.2.2\n   || ==8.4.4\n   || ==8.6.5\n   || ==8.8.4\n   || ==8.10.7\n   || ==9.0.2\n   || ==9.2.3\n\ndescription:\n  This package provides functionality for manipulating @FilePath@ values, and is shipped with <https://www.haskell.org/ghc/ GHC>. It provides two variants for filepaths:\n  .\n  1. legacy filepaths: @type FilePath = String@\n  .\n  2. operating system abstracted filepaths (@OsPath@): internally unpinned @ShortByteString@ (platform-dependent encoding)\n  .\n  It is recommended to use @OsPath@ when possible, because it is more correct.\n  .\n  For each variant there are three main modules:\n  .\n  * \"System.FilePath.Posix\" / \"System.OsPath.Posix\" manipulates POSIX\\/Linux style @FilePath@ values (with @\\/@ as the path separator).\n  .\n  * \"System.FilePath.Windows\" / \"System.OsPath.Windows\" manipulates Windows style @FilePath@ values (with either @\\\\@ or @\\/@ as the path separator, and deals with drives).\n  .\n  * \"System.FilePath\" / \"System.OsPath\" for dealing with current platform-specific filepaths\n  .\n  \"System.OsString\" is like \"System.OsPath\", but more general purpose. Refer to the documentation of\n  those modules for more information.\n  .\n  An introduction into the new API can be found in this\n  <https://hasufell.github.io/posts/2022-06-29-fixing-haskell-filepaths.html blog post>.\n  Code examples for the new API can be found <https://github.com/hasufell/filepath-examples here>.\n\nextra-source-files:\n  Generate.hs\n  Makefile\n  System/FilePath/Internal.hs\n  System/OsPath/Common.hs\n  System/OsString/Common.hs\n  tests/bytestring-tests/Properties/Common.hs\n\nextra-doc-files:\n  changelog.md\n  HACKING.md\n  README.md\n\nflag cpphs\n  description: Use cpphs (fixes haddock source links)\n  default:     False\n  manual:      True\n\nsource-repository head\n  type:     git\n  location: https://github.com/haskell/filepath\n\nlibrary\n  exposed-modules:\n    System.FilePath\n    System.FilePath.Posix\n    System.FilePath.Windows\n    System.OsPath\n    System.OsPath.Data.ByteString.Short\n    System.OsPath.Data.ByteString.Short.Internal\n    System.OsPath.Data.ByteString.Short.Word16\n    System.OsPath.Encoding\n    System.OsPath.Encoding.Internal\n    System.OsPath.Internal\n    System.OsPath.Posix\n    System.OsPath.Posix.Internal\n    System.OsPath.Types\n    System.OsPath.Windows\n    System.OsPath.Windows.Internal\n    System.OsString\n    System.OsString.Internal\n    System.OsString.Internal.Types\n    System.OsString.Posix\n    System.OsString.Windows\n\n  other-extensions:\n    CPP\n    PatternGuards\n\n  if impl(ghc >=7.2)\n    other-extensions: Safe\n\n  default-language: Haskell2010\n  build-depends:\n    , base              >=4.9      && <4.19\n    , bytestring        >=0.11.3.0\n    , deepseq\n    , exceptions\n    , template-haskell\n\n  ghc-options:      -Wall\n\n  if flag(cpphs)\n    ghc-options:        -pgmPcpphs -optP--cpp\n    build-tool-depends: cpphs:cpphs -any\n\ntest-suite filepath-tests\n  type:             exitcode-stdio-1.0\n  main-is:          Test.hs\n  hs-source-dirs:   tests tests/filepath-tests\n  other-modules:\n    TestGen\n    TestUtil\n\n  build-depends:\n    , base\n    , bytestring  >=0.11.3.0\n    , filepath\n    , QuickCheck  >=2.7      && <2.15\n\n  default-language: Haskell2010\n  ghc-options:      -Wall\n\ntest-suite filepath-equivalent-tests\n  default-language: Haskell2010\n  ghc-options:      -Wall\n  type:             exitcode-stdio-1.0\n  main-is:          TestEquiv.hs\n  hs-source-dirs:   tests tests/filepath-equivalent-tests\n  other-modules:\n    Legacy.System.FilePath\n    Legacy.System.FilePath.Posix\n    Legacy.System.FilePath.Windows\n    TestUtil\n\n  build-depends:\n    , base\n    , bytestring  >=0.11.3.0\n    , filepath\n    , QuickCheck  >=2.7      && <2.15\n\ntest-suite bytestring-tests\n  default-language: Haskell2010\n  ghc-options:      -Wall\n  type:             exitcode-stdio-1.0\n  main-is:          Main.hs\n  hs-source-dirs:   tests tests/bytestring-tests\n  other-modules:\n    Properties.ShortByteString\n    Properties.ShortByteString.Word16\n    TestUtil\n\n  build-depends:\n    , base\n    , bytestring  >=0.11.3.0\n    , filepath\n    , QuickCheck  >=2.7      && <2.15\n\ntest-suite abstract-filepath\n  default-language: Haskell2010\n  ghc-options:      -Wall\n  type:             exitcode-stdio-1.0\n  main-is:          Test.hs\n  hs-source-dirs:   tests tests/abstract-filepath\n  other-modules:\n    Arbitrary\n    EncodingSpec\n    OsPathSpec\n    TestUtil\n\n  build-depends:\n    , base\n    , bytestring  >=0.11.3.0\n    , checkers    ^>=0.5.6\n    , deepseq\n    , filepath\n    , QuickCheck  >=2.7      && <2.15\n\nbenchmark bench-filepath\n  default-language: Haskell2010\n  ghc-options:      -Wall\n  type:             exitcode-stdio-1.0\n  main-is:          BenchFilePath.hs\n  hs-source-dirs:   bench\n  other-modules:    TastyBench\n  build-depends:\n    , base\n    , bytestring  >=0.11.3.0\n    , deepseq\n    , filepath\n\n  if impl(ghc >=8.10)\n    ghc-options: \"-with-rtsopts=-A32m --nonmoving-gc\"\n\n  else\n    ghc-options: -with-rtsopts=-A32m\n";
    }