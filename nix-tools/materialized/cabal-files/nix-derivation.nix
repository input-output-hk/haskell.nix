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
      identifier = { name = "nix-derivation"; version = "1.1.2"; };
      license = "BSD-3-Clause";
      copyright = "2017 Gabriella Gonzalez";
      maintainer = "GenuineGabriella@gmail.com";
      author = "Gabriella Gonzalez";
      homepage = "";
      url = "";
      synopsis = "Parse and render *.drv files";
      description = "Use this package to parse and render Nix derivation files (i.e. *.drv files)\n\nThis package also provides a @pretty-derivation@ executable which reads a\nderivation on standard input and outputs the pretty-printed Haskell\nrepresentation on standard output";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."attoparsec" or (errorHandler.buildDepError "attoparsec"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          ];
        buildable = true;
        };
      exes = {
        "pretty-derivation" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."attoparsec" or (errorHandler.buildDepError "attoparsec"))
            (hsPkgs."pretty-show" or (errorHandler.buildDepError "pretty-show"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."nix-derivation" or (errorHandler.buildDepError "nix-derivation"))
            ];
          buildable = true;
          };
        };
      tests = {
        "example" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."attoparsec" or (errorHandler.buildDepError "attoparsec"))
            (hsPkgs."nix-derivation" or (errorHandler.buildDepError "nix-derivation"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            ];
          buildable = true;
          };
        "property" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."attoparsec" or (errorHandler.buildDepError "attoparsec"))
            (hsPkgs."nix-derivation" or (errorHandler.buildDepError "nix-derivation"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            ];
          buildable = true;
          };
        };
      benchmarks = {
        "benchmark" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."attoparsec" or (errorHandler.buildDepError "attoparsec"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."nix-derivation" or (errorHandler.buildDepError "nix-derivation"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/nix-derivation-1.1.2.tar.gz";
      sha256 = "c7ff162f245021d7ba8ea24b993b1df2241744f6e0a78b0783092182fbea8808";
      });
    }) // {
    package-description-override = "Name: nix-derivation\r\nVersion: 1.1.2\r\nx-revision: 4\r\nCabal-Version: >=1.10\r\nBuild-Type: Simple\r\nTested-With: GHC == 7.8.4, GHC == 7.10.3, GHC == 8.0.2, GHC == 8.8.3\r\nLicense: BSD3\r\nLicense-File: LICENSE\r\nCopyright: 2017 Gabriella Gonzalez\r\nAuthor: Gabriella Gonzalez\r\nMaintainer: GenuineGabriella@gmail.com\r\nBug-Reports: https://github.com/Gabriella439/Haskell-Nix-Derivation-Library/issues\r\nSynopsis: Parse and render *.drv files\r\nDescription:\r\n    Use this package to parse and render Nix derivation files (i.e. *.drv files)\r\n    .\r\n    This package also provides a @pretty-derivation@ executable which reads a\r\n    derivation on standard input and outputs the pretty-printed Haskell\r\n    representation on standard output\r\nCategory: System\r\nExtra-Source-Files:\r\n    tests/example0.drv\r\n    tests/example1.drv\r\nSource-Repository head\r\n    Type: git\r\n    Location: https://github.com/Gabriella439/Haskell-Nix-Derivation-Library\r\n\r\nLibrary\r\n    Default-Language: Haskell2010\r\n    Hs-Source-Dirs: src\r\n    Build-Depends:\r\n        base            >= 4.6.0.0  && < 5   ,\r\n        attoparsec      >= 0.12.0.0 && < 0.15,\r\n        containers                     < 0.7 ,\r\n        deepseq         >= 1.4.0.0  && < 1.5 ,\r\n        text            >= 0.8.0.0  && < 2.1 ,\r\n        vector                         < 0.14,\r\n        filepath                       < 1.5\r\n    Exposed-Modules:\r\n        Nix.Derivation\r\n    Other-Modules:\r\n        Nix.Derivation.Builder,\r\n        Nix.Derivation.Parser,\r\n        Nix.Derivation.Types\r\n    GHC-Options: -Wall\r\n\r\nExecutable pretty-derivation\r\n    Default-Language: Haskell2010\r\n    Hs-Source-Dirs: pretty-derivation\r\n    Build-Depends:\r\n        base           >= 4.6.0.0  && < 5   ,\r\n        attoparsec     >= 0.12.0.0 && < 0.15,\r\n        pretty-show    >= 1.6.11   && < 1.11,\r\n        text                                ,\r\n        nix-derivation\r\n    GHC-Options: -Wall\r\n    Main-Is: Main.hs\r\n\r\nTest-Suite example\r\n    Default-Language: Haskell2010\r\n    Type: exitcode-stdio-1.0\r\n    Hs-Source-Dirs: tests\r\n    Main-Is: Example.hs\r\n    GHC-Options: -Wall\r\n    Build-Depends:\r\n        base           >= 4.6.0.0  && < 5   ,\r\n        attoparsec     >= 0.12.0.0 && < 0.15,\r\n        nix-derivation                      ,\r\n        text\r\n\r\nTest-Suite property\r\n    Default-Language: Haskell2010\r\n    Type: exitcode-stdio-1.0\r\n    Hs-Source-Dirs: tests\r\n    Main-Is: Property.hs\r\n    GHC-Options: -Wall\r\n    Build-Depends:\r\n        base            >= 4.6.0.0  && < 5   ,\r\n        attoparsec      >= 0.12.0.0 && < 0.15,\r\n        nix-derivation                       ,\r\n        QuickCheck                     < 2.15,\r\n        text                                 ,\r\n        vector                         < 0.14,\r\n        filepath                       < 1.5\r\n\r\nBenchmark benchmark\r\n    Default-Language: Haskell2010\r\n    Type:             exitcode-stdio-1.0\r\n    HS-Source-Dirs:   bench\r\n    Main-Is:          Main.hs\r\n    GHC-Options:      -Wall\r\n\r\n    Build-Depends:\r\n        base            >= 4.6.0.0  && < 5   ,\r\n        attoparsec      >= 0.12.0.0 && < 0.15,\r\n        criterion       >= 1.1.4.0  && < 1.7 ,\r\n        nix-derivation                       ,\r\n        text\r\n";
    }