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
      identifier = { name = "ansi-terminal-types"; version = "0.11.5"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Mike Pilgrem <public@pilgrem.com>, Roman Cheplyaka <roma@ro-che.info>";
      author = "Max Bolingbroke";
      homepage = "https://github.com/UnkindPartition/ansi-terminal";
      url = "";
      synopsis = "Types and functions used to represent SGR aspects";
      description = "The \\'ANSI\\' standards refer to the visual style of\ndisplaying characters as their \\'graphic rendition\\'. The\n\\'ANSI\\' codes to establish the graphic rendition for\nsubsequent text are referred to as SELECT GRAPHIC RENDITION\n(SGR). This package exposes modules that export types and\nfunctions used to represent SGR aspects.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."colour" or (errorHandler.buildDepError "colour"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/ansi-terminal-types-0.11.5.tar.gz";
      sha256 = "bf7b230389f43105100a6d5740822598a88b38eaaf7d042de595ccf72db4fdd1";
      });
    }) // {
    package-description-override = "Name:                ansi-terminal-types\r\nVersion:             0.11.5\r\nx-revision: 1\r\nCabal-Version:       >= 1.10\r\nCategory:            User Interfaces\r\nSynopsis:            Types and functions used to represent SGR aspects\r\nDescription:         The \\'ANSI\\' standards refer to the visual style of\r\n                     displaying characters as their \\'graphic rendition\\'. The\r\n                     \\'ANSI\\' codes to establish the graphic rendition for\r\n                     subsequent text are referred to as SELECT GRAPHIC RENDITION\r\n                     (SGR). This package exposes modules that export types and\r\n                     functions used to represent SGR aspects.\r\nLicense:             BSD3\r\nLicense-File:        LICENSE\r\nAuthor:              Max Bolingbroke\r\nMaintainer:          Mike Pilgrem <public@pilgrem.com>, Roman Cheplyaka <roma@ro-che.info>\r\nHomepage:            https://github.com/UnkindPartition/ansi-terminal\r\nBuild-Type:          Simple\r\n\r\nExtra-Source-Files:     CHANGELOG.md\r\n                        README.md\r\n\r\nSource-repository head\r\n  type:     git\r\n  location: git://github.com/UnkindPartition/ansi-terminal.git\r\n\r\nLibrary\r\n        Hs-Source-Dirs:         src\r\n        Exposed-Modules:        System.Console.ANSI.Types\r\n        Build-Depends:          base >= 4.8.0.0 && < 5\r\n                              , colour >=2.1.0\r\n        Default-Extensions:     CPP\r\n        Ghc-Options:            -Wall\r\n        Default-Language:       Haskell2010\r\n";
    }