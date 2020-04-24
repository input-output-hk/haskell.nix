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
    flags = { ghc-lib = false; };
    package = {
      specVersion = "1.20";
      identifier = { name = "ghcide"; version = "0.1.0"; };
      license = "Apache-2.0";
      copyright = "Digital Asset 2018-2019";
      maintainer = "Digital Asset";
      author = "Digital Asset";
      homepage = "https://github.com/digital-asset/ghcide#readme";
      url = "";
      synopsis = "The core of an IDE";
      description = "A library for building Haskell IDE's on top of the GHC API.";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" ];
      dataDir = "";
      dataFiles = [];
      extraSrcFiles = [
        "include/ghc-api-version.h"
        "README.md"
        "CHANGELOG.md"
        "test/data/GotoHover.hs"
        ];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = ([
          (hsPkgs."aeson" or (buildDepError "aeson"))
          (hsPkgs."array" or (buildDepError "array"))
          (hsPkgs."async" or (buildDepError "async"))
          (hsPkgs."base" or (buildDepError "base"))
          (hsPkgs."binary" or (buildDepError "binary"))
          (hsPkgs."bytestring" or (buildDepError "bytestring"))
          (hsPkgs."containers" or (buildDepError "containers"))
          (hsPkgs."data-default" or (buildDepError "data-default"))
          (hsPkgs."deepseq" or (buildDepError "deepseq"))
          (hsPkgs."directory" or (buildDepError "directory"))
          (hsPkgs."extra" or (buildDepError "extra"))
          (hsPkgs."fuzzy" or (buildDepError "fuzzy"))
          (hsPkgs."filepath" or (buildDepError "filepath"))
          (hsPkgs."haddock-library" or (buildDepError "haddock-library"))
          (hsPkgs."hashable" or (buildDepError "hashable"))
          (hsPkgs."haskell-lsp-types" or (buildDepError "haskell-lsp-types"))
          (hsPkgs."haskell-lsp" or (buildDepError "haskell-lsp"))
          (hsPkgs."mtl" or (buildDepError "mtl"))
          (hsPkgs."network-uri" or (buildDepError "network-uri"))
          (hsPkgs."prettyprinter-ansi-terminal" or (buildDepError "prettyprinter-ansi-terminal"))
          (hsPkgs."prettyprinter-ansi-terminal" or (buildDepError "prettyprinter-ansi-terminal"))
          (hsPkgs."prettyprinter" or (buildDepError "prettyprinter"))
          (hsPkgs."regex-tdfa" or (buildDepError "regex-tdfa"))
          (hsPkgs."rope-utf16-splay" or (buildDepError "rope-utf16-splay"))
          (hsPkgs."safe-exceptions" or (buildDepError "safe-exceptions"))
          (hsPkgs."shake" or (buildDepError "shake"))
          (hsPkgs."sorted-list" or (buildDepError "sorted-list"))
          (hsPkgs."stm" or (buildDepError "stm"))
          (hsPkgs."syb" or (buildDepError "syb"))
          (hsPkgs."text" or (buildDepError "text"))
          (hsPkgs."time" or (buildDepError "time"))
          (hsPkgs."transformers" or (buildDepError "transformers"))
          (hsPkgs."unordered-containers" or (buildDepError "unordered-containers"))
          (hsPkgs."utf8-string" or (buildDepError "utf8-string"))
          (hsPkgs."hslogger" or (buildDepError "hslogger"))
          ] ++ (if flags.ghc-lib
          then [
            (hsPkgs."ghc-lib" or (buildDepError "ghc-lib"))
            (hsPkgs."ghc-lib-parser" or (buildDepError "ghc-lib-parser"))
            ]
          else [
            (hsPkgs."ghc-boot-th" or (buildDepError "ghc-boot-th"))
            (hsPkgs."ghc-boot" or (buildDepError "ghc-boot"))
            (hsPkgs."ghc" or (buildDepError "ghc"))
            ])) ++ (pkgs.lib).optional (!system.isWindows) (hsPkgs."unix" or (buildDepError "unix"));
        buildable = true;
        modules = [
          "Development/IDE/Core/Compile"
          "Development/IDE/Core/Preprocessor"
          "Development/IDE/Core/FileExists"
          "Development/IDE/GHC/Compat"
          "Development/IDE/GHC/CPP"
          "Development/IDE/GHC/Orphans"
          "Development/IDE/GHC/Warnings"
          "Development/IDE/Import/FindImports"
          "Development/IDE/LSP/HoverDefinition"
          "Development/IDE/LSP/Notifications"
          "Development/IDE/LSP/Outline"
          "Development/IDE/Spans/AtPoint"
          "Development/IDE/Spans/Calculate"
          "Development/IDE/Spans/Documentation"
          "Development/IDE/Spans/Type"
          "Development/IDE/Plugin/CodeAction/PositionIndexed"
          "Development/IDE/Plugin/CodeAction/Rules"
          "Development/IDE/Plugin/CodeAction/RuleTypes"
          "Development/IDE/Plugin/Completions/Logic"
          "Development/IDE/Plugin/Completions/Types"
          "Development/IDE/Core/Debouncer"
          "Development/IDE/Core/FileStore"
          "Development/IDE/Core/IdeConfiguration"
          "Development/IDE/Core/OfInterest"
          "Development/IDE/Core/PositionMapping"
          "Development/IDE/Core/Rules"
          "Development/IDE/Core/RuleTypes"
          "Development/IDE/Core/Service"
          "Development/IDE/Core/Shake"
          "Development/IDE/GHC/Error"
          "Development/IDE/GHC/Util"
          "Development/IDE/Import/DependencyInformation"
          "Development/IDE/LSP/LanguageServer"
          "Development/IDE/LSP/Protocol"
          "Development/IDE/LSP/Server"
          "Development/IDE/Spans/Common"
          "Development/IDE/Types/Diagnostics"
          "Development/IDE/Types/Location"
          "Development/IDE/Types/Logger"
          "Development/IDE/Types/Options"
          "Development/IDE/Plugin"
          "Development/IDE/Plugin/Completions"
          "Development/IDE/Plugin/CodeAction"
          ] ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).gt "8.7" || flags.ghc-lib) "Development/IDE/GHC/HieAst";
        cSources = (pkgs.lib).optional (!system.isWindows) "cbits/getmodtime.c";
        hsSourceDirs = [ "src" ];
        includeDirs = [ "include" ];
        };
      exes = {
        "ghcide-test-preprocessor" = {
          depends = [ (hsPkgs."base" or (buildDepError "base")) ];
          buildable = true;
          hsSourceDirs = [ "test/preprocessor" ];
          mainPath = [ "Main.hs" ];
          };
        "ghcide" = {
          depends = [
            (hsPkgs."hslogger" or (buildDepError "hslogger"))
            (hsPkgs."aeson" or (buildDepError "aeson"))
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."binary" or (buildDepError "binary"))
            (hsPkgs."base16-bytestring" or (buildDepError "base16-bytestring"))
            (hsPkgs."bytestring" or (buildDepError "bytestring"))
            (hsPkgs."containers" or (buildDepError "containers"))
            (hsPkgs."cryptohash-sha1" or (buildDepError "cryptohash-sha1"))
            (hsPkgs."data-default" or (buildDepError "data-default"))
            (hsPkgs."deepseq" or (buildDepError "deepseq"))
            (hsPkgs."directory" or (buildDepError "directory"))
            (hsPkgs."extra" or (buildDepError "extra"))
            (hsPkgs."filepath" or (buildDepError "filepath"))
            (hsPkgs."ghc-paths" or (buildDepError "ghc-paths"))
            (hsPkgs."ghc" or (buildDepError "ghc"))
            (hsPkgs."gitrev" or (buildDepError "gitrev"))
            (hsPkgs."hashable" or (buildDepError "hashable"))
            (hsPkgs."haskell-lsp" or (buildDepError "haskell-lsp"))
            (hsPkgs."haskell-lsp-types" or (buildDepError "haskell-lsp-types"))
            (hsPkgs."hie-bios" or (buildDepError "hie-bios"))
            (hsPkgs."ghcide" or (buildDepError "ghcide"))
            (hsPkgs."optparse-applicative" or (buildDepError "optparse-applicative"))
            (hsPkgs."shake" or (buildDepError "shake"))
            (hsPkgs."text" or (buildDepError "text"))
            (hsPkgs."unordered-containers" or (buildDepError "unordered-containers"))
            ];
          buildable = if flags.ghc-lib then false else true;
          modules = [ "Arguments" "Paths_ghcide" ];
          hsSourceDirs = [ "exe" ];
          mainPath = [ "Main.hs" ] ++ (pkgs.lib).optional (flags.ghc-lib) "";
          };
        };
      tests = {
        "ghcide-tests" = {
          depends = [
            (hsPkgs."aeson" or (buildDepError "aeson"))
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."bytestring" or (buildDepError "bytestring"))
            (hsPkgs."containers" or (buildDepError "containers"))
            (hsPkgs."directory" or (buildDepError "directory"))
            (hsPkgs."extra" or (buildDepError "extra"))
            (hsPkgs."filepath" or (buildDepError "filepath"))
            (hsPkgs."ghc" or (buildDepError "ghc"))
            (hsPkgs."ghcide" or (buildDepError "ghcide"))
            (hsPkgs."ghc-typelits-knownnat" or (buildDepError "ghc-typelits-knownnat"))
            (hsPkgs."haddock-library" or (buildDepError "haddock-library"))
            (hsPkgs."haskell-lsp" or (buildDepError "haskell-lsp"))
            (hsPkgs."haskell-lsp-types" or (buildDepError "haskell-lsp-types"))
            (hsPkgs."lens" or (buildDepError "lens"))
            (hsPkgs."lsp-test" or (buildDepError "lsp-test"))
            (hsPkgs."parser-combinators" or (buildDepError "parser-combinators"))
            (hsPkgs."QuickCheck" or (buildDepError "QuickCheck"))
            (hsPkgs."quickcheck-instances" or (buildDepError "quickcheck-instances"))
            (hsPkgs."rope-utf16-splay" or (buildDepError "rope-utf16-splay"))
            (hsPkgs."shake" or (buildDepError "shake"))
            (hsPkgs."tasty" or (buildDepError "tasty"))
            (hsPkgs."tasty-expected-failure" or (buildDepError "tasty-expected-failure"))
            (hsPkgs."tasty-hunit" or (buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (buildDepError "tasty-quickcheck"))
            (hsPkgs."text" or (buildDepError "text"))
            ];
          build-tools = [
            (hsPkgs.buildPackages.ghcide or (pkgs.buildPackages.ghcide or (buildToolDepError "ghcide")))
            (hsPkgs.buildPackages.ghcide or (pkgs.buildPackages.ghcide or (buildToolDepError "ghcide")))
            ];
          buildable = if flags.ghc-lib then false else true;
          modules = [ "Development/IDE/Test" "Development/IDE/Test/Runfiles" ];
          hsSourceDirs = [ "test/cabal" "test/exe" "test/src" ];
          includeDirs = [ "include" ];
          mainPath = [ "Main.hs" ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../.; }