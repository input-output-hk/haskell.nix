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
      identifier = { name = "gitrev"; version = "1.3.1"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "acfoltzer@galois.com";
      author = "Adam C. Foltzer";
      homepage = "https://github.com/acfoltzer/gitrev";
      url = "";
      synopsis = "Compile git revision info into Haskell projects";
      description = "Some handy Template Haskell splices for including the current git hash and branch in the code of your project. Useful for including in panic messages, @--version@ output, or diagnostic info for more informative bug reports.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."base-compat" or (errorHandler.buildDepError "base-compat"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."process" or (errorHandler.buildDepError "process"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/gitrev-1.3.1.tar.gz";
      sha256 = "a89964db24f56727b0e7b10c98fe7c116d721d8c46f52d6e77088669aaa38332";
      });
    }) // {
    package-description-override = "name:                gitrev\nversion:             1.3.1\nsynopsis:            Compile git revision info into Haskell projects\nhomepage:            https://github.com/acfoltzer/gitrev\nlicense:             BSD3\nlicense-file:        LICENSE\nauthor:              Adam C. Foltzer\nmaintainer:          acfoltzer@galois.com\ncategory:            Development\nbuild-type:          Simple\ncabal-version:       >=1.10\ntested-with:         GHC == 7.6.3, GHC == 7.8.4, GHC == 7.10.3, GHC == 8.0.2\ndescription:         Some handy Template Haskell splices for including the current git hash and branch in the code of your project. Useful for including in panic messages, @--version@ output, or diagnostic info for more informative bug reports.\n\nsource-repository head\n  type:     git\n  location: https://github.com/acfoltzer/gitrev.git\n\nlibrary\n  build-depends:       base >= 4.6 && < 5,\n                       base-compat >= 0.6.0,\n                       directory,\n                       filepath,\n                       template-haskell,\n                       process\n  hs-source-dirs:      src\n  ghc-options:         -Wall\n  default-language:    Haskell2010\n  exposed-modules:     Development.GitRev";
    }