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
    flags = { os-string = false; };
    package = {
      specVersion = "2.4";
      identifier = { name = "file-io"; version = "0.1.4"; };
      license = "BSD-3-Clause";
      copyright = "Julian Ospald 2022";
      maintainer = "hasufell@posteo.de";
      author = "Julian Ospald";
      homepage = "https://github.com/hasufell/file-io";
      url = "";
      synopsis = "Basic file IO operations via 'OsPath'";
      description = "Basic file IO operations like Prelude, but for 'OsPath'.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = ([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
        ] ++ (if system.isWindows
          then [ (hsPkgs."Win32" or (errorHandler.buildDepError "Win32")) ]
          else [
            (hsPkgs."unix" or (errorHandler.buildDepError "unix"))
          ])) ++ (if flags.os-string
          then [
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."os-string" or (errorHandler.buildDepError "os-string"))
          ]
          else [
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          ]);
        buildable = true;
      };
      tests = {
        "T15" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."file-io" or (errorHandler.buildDepError "file-io"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."temporary" or (errorHandler.buildDepError "temporary"))
          ] ++ pkgs.lib.optional (system.isWindows) (hsPkgs."Win32" or (errorHandler.buildDepError "Win32"));
          buildable = true;
        };
        "T15Win" = {
          depends = if system.isWindows
            then [
              (hsPkgs."base" or (errorHandler.buildDepError "base"))
              (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
              (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
              (hsPkgs."file-io" or (errorHandler.buildDepError "file-io"))
              (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
              (hsPkgs."temporary" or (errorHandler.buildDepError "temporary"))
              (hsPkgs."Win32" or (errorHandler.buildDepError "Win32"))
            ]
            else [ (hsPkgs."base" or (errorHandler.buildDepError "base")) ];
          buildable = true;
        };
        "T14" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."file-io" or (errorHandler.buildDepError "file-io"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."temporary" or (errorHandler.buildDepError "temporary"))
          ];
          buildable = true;
        };
        "T8" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."file-io" or (errorHandler.buildDepError "file-io"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."temporary" or (errorHandler.buildDepError "temporary"))
          ];
          buildable = true;
        };
        "CLC237" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."file-io" or (errorHandler.buildDepError "file-io"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."temporary" or (errorHandler.buildDepError "temporary"))
          ];
          buildable = true;
        };
        "Properties" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."file-io" or (errorHandler.buildDepError "file-io"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."temporary" or (errorHandler.buildDepError "temporary"))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/file-io-0.1.4.tar.gz";
      sha256 = "e3d9113a015c57e3d8c2294550c41544f84a265291fed96cca697f91b6e86f52";
    });
  }) // {
    package-description-override = "cabal-version:      2.4\nname:               file-io\nversion:            0.1.4\nsynopsis:           Basic file IO operations via 'OsPath'\ndescription:        Basic file IO operations like Prelude, but for 'OsPath'.\nhomepage:           https://github.com/hasufell/file-io\nbug-reports:        https://github.com/hasufell/file-io/issues\nlicense:            BSD-3-Clause\nauthor:             Julian Ospald\nmaintainer:         hasufell@posteo.de\ncopyright:          Julian Ospald 2022\ncategory:           System\nextra-source-files:\n  CHANGELOG.md\ntested-with:    GHC==9.8.1,\n                GHC==9.4.8,\n                GHC==9.2.8,\n                GHC==9.0.2,\n                GHC==8.10.7,\n                GHC==8.8.4\n\nsource-repository head\n  type:     git\n  location: https://github.com/hasufell/file-io.git\n\nflag os-string\n  description: Use the new os-string package\n  default: False\n  manual: False\n\nlibrary\n  default-language: Haskell2010\n\n  if os(windows)\n    hs-source-dirs: windows\n    build-depends: Win32 >=2.13.3.0\n\n  else\n    hs-source-dirs: posix\n    build-depends: unix >=2.8.0.0\n\n  hs-source-dirs: .\n  build-depends:\n    , base        >=4.13.0.0 && <5\n    , bytestring  >=0.11.3.0\n    , deepseq\n\n  if flag(os-string)\n    build-depends: filepath >= 1.5.0.0, os-string >= 2.0.0\n  else\n    build-depends: filepath >= 1.4.100.0 && < 1.5.0.0\n\n  exposed-modules:\n    System.File.OsPath\n    System.File.OsPath.Internal\n    System.File.PlatformPath\n    System.File.PlatformPath.Internal\n\n  other-modules: System.File.Platform\n\n  ghc-options:      -Wall\n\ntest-suite T15\n    hs-source-dirs: tests\n    main-is: T15.hs\n    type: exitcode-stdio-1.0\n    default-language: Haskell2010\n    build-depends: base >=4.13.0.0 && <5, tasty, tasty-hunit, file-io, filepath, temporary\n    ghc-options: -Wall -threaded -rtsopts \"-with-rtsopts=-N10\"\n    if os(windows)\n      build-depends: Win32 >=2.13.3.0\n\ntest-suite T15Win\n    hs-source-dirs: tests\n    main-is: T15Win.hs\n    type: exitcode-stdio-1.0\n    default-language: Haskell2010\n    if os(windows)\n      build-depends: base >=4.13.0.0 && <5, tasty, tasty-hunit, file-io, filepath, temporary, Win32 >=2.13.3.0\n    else\n      build-depends: base >=4.13.0.0 && <5\n    ghc-options: -Wall -threaded -rtsopts \"-with-rtsopts=-N10\"\n\ntest-suite T14\n    hs-source-dirs: tests\n    main-is: T14.hs\n    type: exitcode-stdio-1.0\n    default-language: Haskell2010\n    build-depends: base >=4.13.0.0 && <5, file-io, filepath, temporary\n    ghc-options: -Wall\n\ntest-suite T8\n    hs-source-dirs: tests\n    main-is: T8.hs\n    type: exitcode-stdio-1.0\n    default-language: Haskell2010\n    build-depends: base >=4.13.0.0 && <5, bytestring, file-io, filepath, temporary\n    ghc-options: -Wall -threaded\n\ntest-suite CLC237\n    hs-source-dirs: tests\n    main-is: CLC237.hs\n    type: exitcode-stdio-1.0\n    default-language: Haskell2010\n    build-depends: base >=4.13.0.0 && <5, file-io, filepath, temporary\n    ghc-options: -Wall\n\ntest-suite Properties\n    hs-source-dirs: tests\n    main-is: Properties.hs\n    type: exitcode-stdio-1.0\n    default-language: Haskell2010\n    build-depends: base >=4.13.0.0 && <5, bytestring, tasty, tasty-hunit, file-io, filepath, temporary\n    ghc-options: -Wall -threaded -rtsopts \"-with-rtsopts=-N10\"\n\n";
  }