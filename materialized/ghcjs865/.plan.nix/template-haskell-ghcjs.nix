{ system
  , compiler
  , flags
  , pkgs
  , hsPkgs
  , pkgconfPkgs
  , errorHandler
  , config
  , ... }:
  {
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = { name = "template-haskell-ghcjs"; version = "2.14.0.0"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "libraries@haskell.org";
      author = "";
      homepage = "";
      url = "";
      synopsis = "Support library for Template Haskell (customized for GHCJS)";
      description = "This package provides modules containing facilities for manipulating\nHaskell source code using Template Haskell.\n\nSee <http://www.haskell.org/haskellwiki/Template_Haskell> for more\ninformation.";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" ];
      dataDir = "";
      dataFiles = [];
      extraSrcFiles = [ "changelog.md" ];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."ghc-boot-th" or (errorHandler.buildDepError "ghc-boot-th"))
          (hsPkgs."pretty" or (errorHandler.buildDepError "pretty"))
          ];
        buildable = true;
        modules = [
          "Language/Haskell/TH/Lib/Map"
          "Language/Haskell/TH"
          "Language/Haskell/TH/Lib"
          "Language/Haskell/TH/Ppr"
          "Language/Haskell/TH/PprLib"
          "Language/Haskell/TH/Quote"
          "Language/Haskell/TH/Syntax"
          "Language/Haskell/TH/LanguageExtensions"
          "Language/Haskell/TH/Lib/Internal"
          ];
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../lib/template-haskell-ghcjs; }