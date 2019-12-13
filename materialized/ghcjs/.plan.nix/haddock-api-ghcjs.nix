let
  buildDepError = pkg:
    builtins.throw ''
      The Haskell package set does not contain the package: ${pkg} (build dependency).
      
      If you are using Stackage, make sure that you are using a snapshot that contains the package. Otherwise you may need to update the Hackage snapshot you are using, usually by updating haskell.nix.
      '';
  sysDepError = pkg:
    builtins.throw ''
      The Nixpkgs package set does not contain the package: ${pkg} (system dependency).
      
      You may need to augment the system package mapping in haskell.nix so that it can be found.
      '';
  pkgConfDepError = pkg:
    builtins.throw ''
      The pkg-conf packages does not contain the package: ${pkg} (pkg-conf dependency).
      
      You may need to augment the pkg-conf package mapping in haskell.nix so that it can be found.
      '';
  exeDepError = pkg:
    builtins.throw ''
      The local executable components do not include the component: ${pkg} (executable dependency).
      '';
  legacyExeDepError = pkg:
    builtins.throw ''
      The Haskell package set does not contain the package: ${pkg} (executable dependency).
      
      If you are using Stackage, make sure that you are using a snapshot that contains the package. Otherwise you may need to update the Hackage snapshot you are using, usually by updating haskell.nix.
      '';
  buildToolDepError = pkg:
    builtins.throw ''
      Neither the Haskell package set or the Nixpkgs package set contain the package: ${pkg} (build tool dependency).
      
      If this is a system dependency:
      You may need to augment the system package mapping in haskell.nix so that it can be found.
      
      If this is a Haskell dependency:
      If you are using Stackage, make sure that you are using a snapshot that contains the package. Otherwise you may need to update the Hackage snapshot you are using, usually by updating haskell.nix.
      '';
in { system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
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
          (hsPkgs."base" or (buildDepError "base"))
          (hsPkgs."Cabal" or (buildDepError "Cabal"))
          (hsPkgs."ghc-api-ghcjs" or (buildDepError "ghc-api-ghcjs"))
          (hsPkgs."ghc-paths" or (buildDepError "ghc-paths"))
          (hsPkgs."haddock-library-ghcjs" or (buildDepError "haddock-library-ghcjs"))
          (hsPkgs."xhtml" or (buildDepError "xhtml"))
          (hsPkgs."array" or (buildDepError "array"))
          (hsPkgs."bytestring" or (buildDepError "bytestring"))
          (hsPkgs."containers" or (buildDepError "containers"))
          (hsPkgs."deepseq" or (buildDepError "deepseq"))
          (hsPkgs."directory" or (buildDepError "directory"))
          (hsPkgs."filepath" or (buildDepError "filepath"))
          (hsPkgs."ghc-boot" or (buildDepError "ghc-boot"))
          (hsPkgs."transformers" or (buildDepError "transformers"))
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
            (hsPkgs."Cabal" or (buildDepError "Cabal"))
            (hsPkgs."ghc-api-ghcjs" or (buildDepError "ghc-api-ghcjs"))
            (hsPkgs."ghc-paths" or (buildDepError "ghc-paths"))
            (hsPkgs."haddock-library-ghcjs" or (buildDepError "haddock-library-ghcjs"))
            (hsPkgs."xhtml" or (buildDepError "xhtml"))
            (hsPkgs."hspec" or (buildDepError "hspec"))
            (hsPkgs."QuickCheck" or (buildDepError "QuickCheck"))
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."array" or (buildDepError "array"))
            (hsPkgs."bytestring" or (buildDepError "bytestring"))
            (hsPkgs."containers" or (buildDepError "containers"))
            (hsPkgs."deepseq" or (buildDepError "deepseq"))
            (hsPkgs."directory" or (buildDepError "directory"))
            (hsPkgs."filepath" or (buildDepError "filepath"))
            (hsPkgs."ghc-boot" or (buildDepError "ghc-boot"))
            (hsPkgs."transformers" or (buildDepError "transformers"))
            ];
          build-tools = [
            (hsPkgs.buildPackages.hspec-discover or (pkgs.buildPackages.hspec-discover or (buildToolDepError "hspec-discover")))
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