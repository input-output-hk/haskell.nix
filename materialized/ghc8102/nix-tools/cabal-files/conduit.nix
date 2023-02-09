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
      identifier = { name = "conduit"; version = "1.3.4.3"; };
      license = "MIT";
      copyright = "";
      maintainer = "michael@snoyman.com";
      author = "Michael Snoyman";
      homepage = "http://github.com/snoyberg/conduit";
      url = "";
      synopsis = "Streaming data processing library.";
      description = "`conduit` is a solution to the streaming data problem, allowing for production,\ntransformation, and consumption of streams of data in constant memory. It is an\nalternative to lazy I\\/O which guarantees deterministic resource handling.\n\nFor more information about conduit in general, and how this package in\nparticular fits into the ecosystem, see [the conduit\nhomepage](https://github.com/snoyberg/conduit#readme).\n\nHackage documentation generation is not reliable. For up to date documentation, please see: <http://www.stackage.org/package/conduit>.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."resourcet" or (errorHandler.buildDepError "resourcet"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
          (hsPkgs."unliftio-core" or (errorHandler.buildDepError "unliftio-core"))
          (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
          (hsPkgs."mono-traversable" or (errorHandler.buildDepError "mono-traversable"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          ] ++ (if system.isWindows
          then [ (hsPkgs."Win32" or (errorHandler.buildDepError "Win32")) ]
          else [ (hsPkgs."unix" or (errorHandler.buildDepError "unix")) ]);
        buildable = true;
        };
      tests = {
        "conduit-test" = {
          depends = [
            (hsPkgs."conduit" or (errorHandler.buildDepError "conduit"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."resourcet" or (errorHandler.buildDepError "resourcet"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
            (hsPkgs."safe" or (errorHandler.buildDepError "safe"))
            (hsPkgs."split" or (errorHandler.buildDepError "split"))
            (hsPkgs."mono-traversable" or (errorHandler.buildDepError "mono-traversable"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."silently" or (errorHandler.buildDepError "silently"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."unliftio" or (errorHandler.buildDepError "unliftio"))
            ];
          buildable = true;
          };
        };
      benchmarks = {
        "optimize-201408" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."conduit" or (errorHandler.buildDepError "conduit"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."mwc-random" or (errorHandler.buildDepError "mwc-random"))
            (hsPkgs."gauge" or (errorHandler.buildDepError "gauge"))
            (hsPkgs."kan-extensions" or (errorHandler.buildDepError "kan-extensions"))
            ];
          buildable = true;
          };
        "unfused" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."conduit" or (errorHandler.buildDepError "conduit"))
            (hsPkgs."gauge" or (errorHandler.buildDepError "gauge"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/conduit-1.3.4.3.tar.gz";
      sha256 = "aca2a086a6ee065a5d1f1efc0632bccf52a8f961e4134a4fb60447765987907d";
      });
    }) // {
    package-description-override = "Name:                conduit\r\nVersion:             1.3.4.3\r\nx-revision: 1\r\nSynopsis:            Streaming data processing library.\r\ndescription:\r\n    `conduit` is a solution to the streaming data problem, allowing for production,\r\n    transformation, and consumption of streams of data in constant memory. It is an\r\n    alternative to lazy I\\/O which guarantees deterministic resource handling.\r\n    .\r\n    For more information about conduit in general, and how this package in\r\n    particular fits into the ecosystem, see [the conduit\r\n    homepage](https://github.com/snoyberg/conduit#readme).\r\n    .\r\n    Hackage documentation generation is not reliable. For up to date documentation, please see: <http://www.stackage.org/package/conduit>.\r\nLicense:             MIT\r\nLicense-file:        LICENSE\r\nAuthor:              Michael Snoyman\r\nMaintainer:          michael@snoyman.com\r\nCategory:            Data, Conduit\r\nBuild-type:          Simple\r\nCabal-version:       >=1.10\r\nHomepage:            http://github.com/snoyberg/conduit\r\nextra-source-files:  test/main.hs\r\n                   , test/doctests.hs\r\n                   , test/subdir/dummyfile.txt\r\n                   , README.md\r\n                   , ChangeLog.md\r\n                   , fusion-macros.h\r\n\r\nLibrary\r\n  default-language:    Haskell2010\r\n  hs-source-dirs:      src\r\n  Exposed-modules:     Data.Conduit\r\n                       Data.Conduit.Combinators\r\n                       Data.Conduit.List\r\n                       Data.Conduit.Internal\r\n                       Data.Conduit.Lift\r\n                       Data.Conduit.Internal.Fusion\r\n                       Data.Conduit.Internal.List.Stream\r\n                       Data.Conduit.Combinators.Stream\r\n                       Conduit\r\n  other-modules:       Data.Conduit.Internal.Pipe\r\n                       Data.Conduit.Internal.Conduit\r\n                       Data.Conduit.Combinators.Unqualified\r\n                       Data.Streaming.FileRead\r\n                       Data.Streaming.Filesystem\r\n  Build-depends:       base                     >= 4.12         && < 5\r\n                     , resourcet                >= 1.2          && < 1.4\r\n                     , transformers             >= 0.4\r\n                     , mtl\r\n                     , primitive\r\n                     , unliftio-core\r\n                     , exceptions\r\n                     , mono-traversable         >= 1.0.7\r\n                     , vector\r\n                     , bytestring\r\n                     , text\r\n                     , filepath\r\n                     , directory\r\n\r\n  if os(windows)\r\n    build-depends:     Win32\r\n    other-modules:     System.Win32File\r\n    cpp-options:       -DWINDOWS\r\n  else\r\n    build-depends:     unix\r\n\r\n  ghc-options:         -Wall\r\n  include-dirs:        .\r\n\r\ntest-suite conduit-test\r\n    default-language:    Haskell2010\r\n    hs-source-dirs: test\r\n    main-is: main.hs\r\n    other-modules: Data.Conduit.Extra.ZipConduitSpec\r\n                 , Data.Conduit.StreamSpec\r\n                 , Spec\r\n                 , StreamSpec\r\n    type: exitcode-stdio-1.0\r\n    cpp-options:   -DTEST\r\n    build-depends:   conduit\r\n                   , base\r\n                   , hspec >= 1.3\r\n                   , QuickCheck >= 2.7\r\n                   , transformers\r\n                   , mtl\r\n                   , resourcet\r\n                   , containers\r\n                   , exceptions >= 0.6\r\n                   , safe\r\n                   , split >= 0.2.0.0\r\n                   , mono-traversable\r\n                   , text\r\n                   , vector\r\n                   , directory\r\n                   , bytestring\r\n                   , silently\r\n                   , filepath\r\n                   , unliftio >= 0.2.4.0\r\n    ghc-options:     -Wall\r\n\r\n  if os(windows)\r\n    cpp-options:     -DWINDOWS\r\n\r\n--test-suite doctests\r\n--    hs-source-dirs: test\r\n--    main-is: doctests.hs\r\n--    type: exitcode-stdio-1.0\r\n--    ghc-options: -threaded\r\n--    build-depends: base, directory, doctest >= 0.8\r\n\r\n-- benchmark utf8-memory-usage\r\n--     type: exitcode-stdio-1.0\r\n--     hs-source-dirs: benchmarks\r\n--     build-depends:  base\r\n--                   , text-stream-decode\r\n--                   , bytestring\r\n--                   , text\r\n--                   , conduit\r\n--     main-is:        utf8-memory-usage.hs\r\n--     ghc-options:    -Wall -O2 -with-rtsopts=-s\r\n\r\nbenchmark optimize-201408\r\n    default-language:    Haskell2010\r\n    type: exitcode-stdio-1.0\r\n    hs-source-dirs: benchmarks\r\n    build-depends:  base\r\n                  , conduit\r\n                  , vector\r\n                  , deepseq\r\n                  , containers\r\n                  , transformers\r\n                  , hspec\r\n                  , mwc-random\r\n                  , gauge\r\n                  , kan-extensions\r\n    main-is:        optimize-201408.hs\r\n    ghc-options:    -Wall -O2 -rtsopts\r\n\r\nbenchmark unfused\r\n    default-language:    Haskell2010\r\n    type: exitcode-stdio-1.0\r\n    hs-source-dirs: benchmarks\r\n    build-depends:  base\r\n                  , conduit\r\n                  , gauge\r\n                  , transformers\r\n    main-is:        unfused.hs\r\n    ghc-options:    -Wall -O2 -rtsopts\r\n\r\nsource-repository head\r\n  type:     git\r\n  location: git://github.com/snoyberg/conduit.git\r\n";
    }