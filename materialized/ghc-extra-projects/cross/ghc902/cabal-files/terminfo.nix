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
      identifier = { name = "terminfo"; version = "0.4.1.6"; };
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
      url = "http://hackage.haskell.org/package/terminfo-0.4.1.6.tar.gz";
      sha256 = "63d54abe02fe07723e8a12f39d668478ea56ad78c9ef69613064420ba3c0a04f";
      });
    }) // {
    package-description-override = "Name:           terminfo\nCabal-Version:  >=1.10\nVersion:        0.4.1.6\nCategory:       User Interfaces\nLicense:        BSD3\nLicense-File:   LICENSE\nCopyright:      (c) Judah Jacobson\nAuthor:         Judah Jacobson\nMaintainer:     Judah Jacobson <judah.jacobson@gmail.com>\nSynopsis:       Haskell bindings to the terminfo library.\nDescription:    This library provides an interface to the terminfo database (via bindings to the\n                curses library).  <http://en.wikipedia.org/wiki/Terminfo Terminfo> allows POSIX\n                systems to interact with a variety of terminals using a standard set of capabilities.\nHomepage:       https://github.com/judah/terminfo\nBug-Reports:    https://github.com/judah/terminfo/issues\nStability:      Stable\nBuild-type:     Configure\n\nextra-source-files: configure.ac configure terminfo.buildinfo.in Changelog\nextra-tmp-files: config.log config.status autom4te.cache terminfo.buildinfo\n\nSource-Repository head\n    type:     git\n    location: https://github.com/judah/terminfo.git\n\nLibrary\n    default-language: Haskell2010\n    other-extensions: CPP, DeriveDataTypeable, FlexibleInstances, ScopedTypeVariables\n    if impl(ghc>=7.3)\n      other-extensions: Safe, Trustworthy\n    build-depends:    base >= 4.9 && < 4.19\n    ghc-options:      -Wall\n    exposed-modules:\n                    System.Console.Terminfo\n                    System.Console.Terminfo.Base\n                    System.Console.Terminfo.Cursor\n                    System.Console.Terminfo.Color\n                    System.Console.Terminfo.Edit\n                    System.Console.Terminfo.Effects\n                    System.Console.Terminfo.Keys\n";
    }