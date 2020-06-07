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
      specVersion = "1.6";
      identifier = { name = "hscolour"; version = "1.24.4"; };
      license = "LicenseRef-LGPL";
      copyright = "2003-2017 Malcolm Wallace; 2006 Bjorn Bringert";
      maintainer = "Malcolm Wallace";
      author = "Malcolm Wallace";
      homepage = "http://code.haskell.org/~malcolm/hscolour/";
      url = "";
      synopsis = "Colourise Haskell code.";
      description = "hscolour is a small Haskell script to colourise Haskell code. It currently\nhas six output formats:\nANSI terminal codes (optionally XTerm-256colour codes),\nHTML 3.2 with <font> tags,\nHTML 4.01 with CSS,\nHTML 4.01 with CSS and mouseover annotations,\nXHTML 1.0 with inline CSS styling,\nLaTeX,\nand mIRC chat codes.";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENCE-LGPL" ];
      dataDir = "";
      dataFiles = [ "hscolour.css" "data/rgb24-example-.hscolour" ];
      extraSrcFiles = [];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          ];
        buildable = true;
        modules = [
          "Language/Haskell/HsColour"
          "Language/Haskell/HsColour/ANSI"
          "Language/Haskell/HsColour/Anchors"
          "Language/Haskell/HsColour/ACSS"
          "Language/Haskell/HsColour/CSS"
          "Language/Haskell/HsColour/Classify"
          "Language/Haskell/HsColour/ColourHighlight"
          "Language/Haskell/HsColour/Colourise"
          "Language/Haskell/HsColour/General"
          "Language/Haskell/HsColour/HTML"
          "Language/Haskell/HsColour/InlineCSS"
          "Language/Haskell/HsColour/LaTeX"
          "Language/Haskell/HsColour/MIRC"
          "Language/Haskell/HsColour/Options"
          "Language/Haskell/HsColour/Output"
          "Language/Haskell/HsColour/TTY"
          ];
        };
      exes = {
        "HsColour" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            ];
          buildable = true;
          mainPath = [ "HsColour.hs" ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../.; }