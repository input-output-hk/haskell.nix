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
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
        ];
        buildable = true;
      };
      exes = {
        "HsColour" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/hscolour-1.24.4.tar.gz";
      sha256 = "243332b082294117f37b2c2c68079fa61af68b36223b3fc07594f245e0e5321d";
    });
  }) // {
    package-description-override = "Name: hscolour\nVersion: 1.24.4\nCopyright: 2003-2017 Malcolm Wallace; 2006 Bjorn Bringert\nMaintainer: Malcolm Wallace\nAuthor: Malcolm Wallace\nHomepage: http://code.haskell.org/~malcolm/hscolour/\nLicense: LGPL\nLicense-file: LICENCE-LGPL\nSynopsis: Colourise Haskell code.\nDescription:\n  hscolour is a small Haskell script to colourise Haskell code. It currently\n  has six output formats: \n  ANSI terminal codes (optionally XTerm-256colour codes),\n  HTML 3.2 with <font> tags,\n  HTML 4.01 with CSS,\n  HTML 4.01 with CSS and mouseover annotations,\n  XHTML 1.0 with inline CSS styling,\n  LaTeX,\n  and mIRC chat codes.\nCategory: Language\nBuild-Type: Simple\nData-files: hscolour.css, data/rgb24-example-.hscolour\nCabal-version: >=1.6\n\n\nLibrary\n  Build-depends: base < 10, containers\n  Exposed-Modules: \n    Language.Haskell.HsColour\n    Language.Haskell.HsColour.ANSI\n    Language.Haskell.HsColour.Anchors\n    Language.Haskell.HsColour.ACSS\n    Language.Haskell.HsColour.CSS\n    Language.Haskell.HsColour.Classify\n    Language.Haskell.HsColour.ColourHighlight\n    Language.Haskell.HsColour.Colourise\n    Language.Haskell.HsColour.General\n    Language.Haskell.HsColour.HTML\n    Language.Haskell.HsColour.InlineCSS\n    Language.Haskell.HsColour.LaTeX\n    Language.Haskell.HsColour.MIRC\n    Language.Haskell.HsColour.Options\n    Language.Haskell.HsColour.Output\n    Language.Haskell.HsColour.TTY\n  --ghc-options: -O -W\n  Extensions: \n\n\nExecutable HsColour\n  Build-depends: base < 10, containers\n  Main-is: HsColour.hs\n  --ghc-options: -O -W\n  Extensions: CPP\n  cpp-options: -DMAJOR=1 -DMINOR=24\n\n\n\nSource-repository head\n  Type    : darcs\n  Location: http://code.haskell.org/~malcolm/hscolour\n";
  }