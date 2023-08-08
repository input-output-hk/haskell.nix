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
    flags = { example = false; };
    package = {
      specVersion = "1.22";
      identifier = { name = "ansi-terminal"; version = "1.0"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Mike Pilgrem <public@pilgrem.com>, Roman Cheplyaka <roma@ro-che.info>";
      author = "Max Bolingbroke";
      homepage = "https://github.com/UnkindPartition/ansi-terminal";
      url = "";
      synopsis = "Simple ANSI terminal support";
      description = "ANSI terminal support for Haskell: allows cursor movement,\nscreen clearing, color output, showing or hiding the\ncursor, and changing the title. Works on UNIX and Windows.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."ansi-terminal-types" or (errorHandler.buildDepError "ansi-terminal-types"))
          (hsPkgs."colour" or (errorHandler.buildDepError "colour"))
          ];
        buildable = true;
        };
      exes = {
        "ansi-terminal-example" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."ansi-terminal" or (errorHandler.buildDepError "ansi-terminal"))
            (hsPkgs."colour" or (errorHandler.buildDepError "colour"))
            ];
          buildable = if !flags.example then false else true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/ansi-terminal-1.0.tar.gz";
      sha256 = "0798a98f79189e62b5ff0aa247ba95fc3845f0f6f1e14ecceb37facfbdd5cfc9";
      });
    }) // {
    package-description-override = "Cabal-Version:       1.22\r\nName:                ansi-terminal\r\nVersion:             1.0\r\nCategory:            User Interfaces\r\nSynopsis:            Simple ANSI terminal support\r\nDescription:         ANSI terminal support for Haskell: allows cursor movement,\r\n                     screen clearing, color output, showing or hiding the\r\n                     cursor, and changing the title. Works on UNIX and Windows.\r\nLicense:             BSD3\r\nLicense-File:        LICENSE\r\nAuthor:              Max Bolingbroke\r\nMaintainer:          Mike Pilgrem <public@pilgrem.com>, Roman Cheplyaka <roma@ro-che.info>\r\nHomepage:            https://github.com/UnkindPartition/ansi-terminal\r\nBuild-Type:          Simple\r\n\r\nExtra-Source-Files:     CHANGELOG.md\r\n                        README.md\r\n                        win/include/errors.h\r\n                        win/include/winternl_compat.h\r\n\r\nSource-repository head\r\n  type:     git\r\n  location: git://github.com/UnkindPartition/ansi-terminal.git\r\n\r\nFlag Example\r\n        Description:    Build the example application\r\n        Default:        False\r\n\r\nLibrary\r\n        Hs-Source-Dirs:         src\r\n        Exposed-Modules:        System.Console.ANSI\r\n                                System.Console.ANSI.Codes\r\n\r\n        -- We re-export all of ansi-terminal-types to aid compatibility for\r\n        -- downstream users.\r\n        Reexported-Modules:     System.Console.ANSI.Types\r\n\r\n        Other-Modules:          System.Console.ANSI.Internal\r\n\r\n        Build-Depends:          base >= 4.8.0.0 && < 5\r\n                              , ansi-terminal-types == 0.11.5\r\n                              , colour >= 2.1.0\r\n        if os(windows)\r\n            Hs-Source-Dirs:     win\r\n            Other-Modules:      System.Console.ANSI.Windows.Foreign\r\n                                System.Console.ANSI.Windows.Win32.Types\r\n                                System.Console.ANSI.Windows.Win32.MinTTY\r\n            Include-Dirs:       win/include\r\n            Includes:           errors.h\r\n                                winternl_compat.h\r\n            C-Sources:          win/c-source/errors.c\r\n        else\r\n            Hs-Source-Dirs:     unix\r\n\r\n        Default-Extensions:     CPP\r\n\r\n        Ghc-Options:            -Wall\r\n        Default-Language:       Haskell2010\r\n\r\nExecutable ansi-terminal-example\r\n        Hs-Source-Dirs:         app\r\n        Main-Is:                Example.hs\r\n        Build-Depends:          base >= 4.8.0.0 && < 5\r\n                              , ansi-terminal\r\n                              , colour\r\n        Ghc-Options:            -Wall\r\n        if !flag(example)\r\n            Buildable:          False\r\n        Default-Language:       Haskell2010\r\n";
    }