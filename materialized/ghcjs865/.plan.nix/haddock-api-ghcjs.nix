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
      specVersion = "2.0";
      identifier = { name = "haddock-api-ghcjs"; version = "2.20.0"; };
      license = "BSD-3-Clause";
      copyright = "(c) Simon Marlow, David Waern";
      maintainer = "Alex Biehl <alexbiehl@gmail.com>, Simon Hengel <sol@typeful.net>, Mateusz Kowalczyk <fuuzetsu@fuuzetsu.co.uk>";
      author = "Simon Marlow, David Waern";
      homepage = "http://www.haskell.org/haddock/";
      url = "";
      synopsis = "A documentation-generation tool for Haskell libraries";
      description = "Haddock is a documentation-generation tool for Haskell\nlibraries";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" ];
      dataDir = "resources";
      dataFiles = [
        "html/quick-jump.min.js"
        "html/haddock-bundle.min.js"
        "html/quick-jump.css"
        "html/solarized.css"
        "html/highlight.js"
        "html/Classic.theme/haskell_icon.gif"
        "html/Classic.theme/minus.gif"
        "html/Classic.theme/plus.gif"
        "html/Classic.theme/xhaddock.css"
        "html/Ocean.std-theme/hslogo-16.png"
        "html/Ocean.std-theme/minus.gif"
        "html/Ocean.std-theme/ocean.css"
        "html/Ocean.std-theme/plus.gif"
        "html/Ocean.std-theme/synopsis.png"
        "latex/haddock.sty"
        ];
      extraSrcFiles = [ "CHANGES.md" ];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."Cabal" or (errorHandler.buildDepError "Cabal"))
          (hsPkgs."ghc-api-ghcjs" or (errorHandler.buildDepError "ghc-api-ghcjs"))
          (hsPkgs."ghc-paths" or (errorHandler.buildDepError "ghc-paths"))
          (hsPkgs."haddock-library-ghcjs" or (errorHandler.buildDepError "haddock-library-ghcjs"))
          (hsPkgs."xhtml" or (errorHandler.buildDepError "xhtml"))
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."ghc-boot" or (errorHandler.buildDepError "ghc-boot"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          ];
        buildable = true;
        modules = [
          "Haddock"
          "Haddock/Interface"
          "Haddock/Interface/Rename"
          "Haddock/Interface/Create"
          "Haddock/Interface/AttachInstances"
          "Haddock/Interface/Json"
          "Haddock/Interface/LexParseRn"
          "Haddock/Interface/ParseModuleHeader"
          "Haddock/Interface/Specialize"
          "Haddock/Parser"
          "Haddock/Utils"
          "Haddock/Utils/Json"
          "Haddock/Backends/Xhtml"
          "Haddock/Backends/Xhtml/Decl"
          "Haddock/Backends/Xhtml/DocMarkup"
          "Haddock/Backends/Xhtml/Layout"
          "Haddock/Backends/Xhtml/Meta"
          "Haddock/Backends/Xhtml/Names"
          "Haddock/Backends/Xhtml/Themes"
          "Haddock/Backends/Xhtml/Types"
          "Haddock/Backends/Xhtml/Utils"
          "Haddock/Backends/LaTeX"
          "Haddock/Backends/HaddockDB"
          "Haddock/Backends/Hoogle"
          "Haddock/Backends/Hyperlinker"
          "Haddock/Backends/Hyperlinker/Ast"
          "Haddock/Backends/Hyperlinker/Parser"
          "Haddock/Backends/Hyperlinker/Renderer"
          "Haddock/Backends/Hyperlinker/Types"
          "Haddock/Backends/Hyperlinker/Utils"
          "Haddock/ModuleTree"
          "Haddock/Types"
          "Haddock/Doc"
          "Haddock/Version"
          "Haddock/InterfaceFile"
          "Haddock/Options"
          "Haddock/GhcUtils"
          "Haddock/Syb"
          "Haddock/Convert"
          "Paths_haddock_api_ghcjs"
          "Documentation/Haddock"
          ];
        hsSourceDirs = [ "src" ];
        };
      tests = {
        "spec" = {
          depends = [
            (hsPkgs."Cabal" or (errorHandler.buildDepError "Cabal"))
            (hsPkgs."ghc-api-ghcjs" or (errorHandler.buildDepError "ghc-api-ghcjs"))
            (hsPkgs."ghc-paths" or (errorHandler.buildDepError "ghc-paths"))
            (hsPkgs."haddock-library-ghcjs" or (errorHandler.buildDepError "haddock-library-ghcjs"))
            (hsPkgs."xhtml" or (errorHandler.buildDepError "xhtml"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."array" or (errorHandler.buildDepError "array"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."ghc-boot" or (errorHandler.buildDepError "ghc-boot"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            ];
          build-tools = [
            (hsPkgs.buildPackages.hspec-discover or (pkgs.buildPackages.hspec-discover or (errorHandler.buildToolDepError "hspec-discover")))
            ];
          buildable = true;
          modules = [
            "Haddock"
            "Haddock/Backends/Hoogle"
            "Haddock/Backends/Hyperlinker"
            "Haddock/Backends/Hyperlinker/Ast"
            "Haddock/Backends/Hyperlinker/Renderer"
            "Haddock/Backends/Hyperlinker/Utils"
            "Haddock/Backends/LaTeX"
            "Haddock/Backends/Xhtml"
            "Haddock/Backends/Xhtml/Decl"
            "Haddock/Backends/Xhtml/DocMarkup"
            "Haddock/Backends/Xhtml/Layout"
            "Haddock/Backends/Xhtml/Meta"
            "Haddock/Backends/Xhtml/Names"
            "Haddock/Backends/Xhtml/Themes"
            "Haddock/Backends/Xhtml/Types"
            "Haddock/Backends/Xhtml/Utils"
            "Haddock/Convert"
            "Haddock/Doc"
            "Haddock/GhcUtils"
            "Haddock/Interface"
            "Haddock/Interface/AttachInstances"
            "Haddock/Interface/Create"
            "Haddock/Interface/Json"
            "Haddock/Interface/LexParseRn"
            "Haddock/Interface/ParseModuleHeader"
            "Haddock/Interface/Rename"
            "Haddock/Interface/Specialize"
            "Haddock/InterfaceFile"
            "Haddock/ModuleTree"
            "Haddock/Options"
            "Haddock/Parser"
            "Haddock/Syb"
            "Haddock/Types"
            "Haddock/Utils"
            "Haddock/Utils/Json"
            "Haddock/Version"
            "Paths_haddock_api_ghcjs"
            "Haddock/Backends/Hyperlinker/ParserSpec"
            "Haddock/Backends/Hyperlinker/Parser"
            "Haddock/Backends/Hyperlinker/Types"
            ];
          hsSourceDirs = [ "test" "src" ];
          mainPath = [ "Spec.hs" ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../lib/haddock-api-ghcjs; }