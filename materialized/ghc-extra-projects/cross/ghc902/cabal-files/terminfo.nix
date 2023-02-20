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
      identifier = { name = "terminfo"; version = "0.4.1.5"; };
      license = "BSD-3-Clause";
      copyright = "(c) Judah Jacobson";
      maintainer = "Judah Jacobson <judah.jacobson@gmail.com>";
      author = "Judah Jacobson";
      homepage = "https://github.com/judah/terminfo";
      url = "";
      synopsis = "Haskell bindings to the terminfo library.";
      description = "This library provides an interface to the terminfo database (via bindings to the\ncurses library).  <http://en.wikipedia.org/wiki/Terminfo Terminfo> allows POSIX\nsystems to interact with a variety of terminals using a standard set of capabilities.";
      buildType = "Configure";
      };
    components = {
      "library" = {
        depends = [ (hsPkgs."base" or (errorHandler.buildDepError "base")) ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/terminfo-0.4.1.5.tar.gz";
      sha256 = "b160211246cfed10b920c859569e8e0f1952013b8d2d4527fd554342ed2c1d68";
      });
    }) // {
    package-description-override = "Name:           terminfo\r\nCabal-Version:  >=1.10\r\nVersion:        0.4.1.5\r\nx-revision: 1\r\nCategory:       User Interfaces\r\nLicense:        BSD3\r\nLicense-File:   LICENSE\r\nCopyright:      (c) Judah Jacobson\r\nAuthor:         Judah Jacobson\r\nMaintainer:     Judah Jacobson <judah.jacobson@gmail.com>\r\nSynopsis:       Haskell bindings to the terminfo library.\r\nDescription:    This library provides an interface to the terminfo database (via bindings to the\r\n                curses library).  <http://en.wikipedia.org/wiki/Terminfo Terminfo> allows POSIX\r\n                systems to interact with a variety of terminals using a standard set of capabilities.\r\nHomepage:       https://github.com/judah/terminfo\r\nBug-Reports:    https://github.com/judah/terminfo/issues\r\nStability:      Stable\r\nBuild-type:     Configure\r\n\r\nextra-source-files: configure.ac configure terminfo.buildinfo.in Changelog\r\nextra-tmp-files: config.log config.status autom4te.cache terminfo.buildinfo\r\n\r\nSource-Repository head\r\n    type:     git\r\n    location: https://github.com/judah/terminfo.git\r\n\r\nLibrary\r\n    default-language: Haskell2010\r\n    other-extensions: CPP, DeriveDataTypeable, FlexibleInstances, ScopedTypeVariables\r\n    if impl(ghc>=7.3)\r\n      other-extensions: Safe, Trustworthy\r\n    build-depends:    base >= 4.9 && < 4.18\r\n    ghc-options:      -Wall\r\n    exposed-modules:\r\n                    System.Console.Terminfo\r\n                    System.Console.Terminfo.Base\r\n                    System.Console.Terminfo.Cursor\r\n                    System.Console.Terminfo.Color\r\n                    System.Console.Terminfo.Edit\r\n                    System.Console.Terminfo.Effects\r\n                    System.Console.Terminfo.Keys\r\n";
    }