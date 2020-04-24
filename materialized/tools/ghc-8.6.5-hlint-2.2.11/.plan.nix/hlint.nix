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
    flags = { threaded = true; gpl = true; ghc-lib = false; };
    package = {
      specVersion = "1.18";
      identifier = { name = "hlint"; version = "2.2.11"; };
      license = "BSD-3-Clause";
      copyright = "Neil Mitchell 2006-2020";
      maintainer = "Neil Mitchell <ndmitchell@gmail.com>";
      author = "Neil Mitchell <ndmitchell@gmail.com>";
      homepage = "https://github.com/ndmitchell/hlint#readme";
      url = "";
      synopsis = "Source code suggestions";
      description = "HLint gives suggestions on how to improve your source code.";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" ];
      dataDir = "data";
      dataFiles = [
        "hlint.yaml"
        "default.yaml"
        "Test.hs"
        "report_template.html"
        "hs-lint.el"
        "hlint.1"
        "hlint.ghci"
        "HLint_QuickCheck.hs"
        "HLint_TypeCheck.hs"
        ];
      extraSrcFiles = [];
      extraTmpFiles = [];
      extraDocFiles = [ "README.md" "CHANGES.txt" ];
      };
    components = {
      "library" = {
        depends = ([
          (hsPkgs."base" or (buildDepError "base"))
          (hsPkgs."process" or (buildDepError "process"))
          (hsPkgs."filepath" or (buildDepError "filepath"))
          (hsPkgs."directory" or (buildDepError "directory"))
          (hsPkgs."containers" or (buildDepError "containers"))
          (hsPkgs."unordered-containers" or (buildDepError "unordered-containers"))
          (hsPkgs."vector" or (buildDepError "vector"))
          (hsPkgs."text" or (buildDepError "text"))
          (hsPkgs."bytestring" or (buildDepError "bytestring"))
          (hsPkgs."transformers" or (buildDepError "transformers"))
          (hsPkgs."file-embed" or (buildDepError "file-embed"))
          (hsPkgs."utf8-string" or (buildDepError "utf8-string"))
          (hsPkgs."data-default" or (buildDepError "data-default"))
          (hsPkgs."cpphs" or (buildDepError "cpphs"))
          (hsPkgs."cmdargs" or (buildDepError "cmdargs"))
          (hsPkgs."yaml" or (buildDepError "yaml"))
          (hsPkgs."haskell-src-exts" or (buildDepError "haskell-src-exts"))
          (hsPkgs."haskell-src-exts-util" or (buildDepError "haskell-src-exts-util"))
          (hsPkgs."uniplate" or (buildDepError "uniplate"))
          (hsPkgs."ansi-terminal" or (buildDepError "ansi-terminal"))
          (hsPkgs."extra" or (buildDepError "extra"))
          (hsPkgs."refact" or (buildDepError "refact"))
          (hsPkgs."aeson" or (buildDepError "aeson"))
          (hsPkgs."filepattern" or (buildDepError "filepattern"))
          (hsPkgs."ghc-lib-parser-ex" or (buildDepError "ghc-lib-parser-ex"))
          ] ++ (if !flags.ghc-lib && (compiler.isGhc && (compiler.version).ge "8.8.0") && (compiler.isGhc && (compiler.version).lt "8.9.0")
          then [
            (hsPkgs."ghc" or (buildDepError "ghc"))
            (hsPkgs."ghc-boot-th" or (buildDepError "ghc-boot-th"))
            (hsPkgs."ghc-boot" or (buildDepError "ghc-boot"))
            ]
          else [
            (hsPkgs."ghc-lib-parser" or (buildDepError "ghc-lib-parser"))
            ])) ++ (pkgs.lib).optional (flags.gpl) (hsPkgs."hscolour" or (buildDepError "hscolour"));
        buildable = true;
        modules = [
          "Paths_hlint"
          "Apply"
          "CmdLine"
          "Grep"
          "HLint"
          "HsColour"
          "Idea"
          "Report"
          "Util"
          "Parallel"
          "Refact"
          "Timing"
          "CC"
          "EmbedData"
          "Config/Compute"
          "Config/Haskell"
          "Config/Read"
          "Config/Type"
          "Config/Yaml"
          "GHC/Util"
          "GHC/Util/ApiAnnotation"
          "GHC/Util/View"
          "GHC/Util/Brackets"
          "GHC/Util/FreeVars"
          "GHC/Util/HsDecl"
          "GHC/Util/HsExpr"
          "GHC/Util/HsType"
          "GHC/Util/Pat"
          "GHC/Util/LanguageExtensions/Type"
          "GHC/Util/Module"
          "GHC/Util/Outputable"
          "GHC/Util/SrcLoc"
          "GHC/Util/DynFlags"
          "GHC/Util/RdrName"
          "GHC/Util/Scope"
          "GHC/Util/Unify"
          "HSE/All"
          "HSE/Match"
          "HSE/Scope"
          "HSE/Type"
          "HSE/Util"
          "Hint/All"
          "Hint/Bracket"
          "Hint/Comment"
          "Hint/Duplicate"
          "Hint/Export"
          "Hint/Extensions"
          "Hint/Import"
          "Hint/Lambda"
          "Hint/List"
          "Hint/ListRec"
          "Hint/Match"
          "Hint/Monad"
          "Hint/Naming"
          "Hint/NewType"
          "Hint/Pattern"
          "Hint/Pragma"
          "Hint/Restrict"
          "Hint/Smell"
          "Hint/Type"
          "Hint/Unsafe"
          "Hint/Util"
          "Test/All"
          "Test/Annotations"
          "Test/InputOutput"
          "Test/Proof"
          "Test/Translate"
          "Test/Util"
          "Language/Haskell/HLint"
          "Language/Haskell/HLint3"
          "Language/Haskell/HLint4"
          ];
        hsSourceDirs = [ "src" ];
        };
      exes = {
        "hlint" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."hlint" or (buildDepError "hlint"))
            ];
          buildable = true;
          mainPath = [
            "src/Main.hs"
            ] ++ (pkgs.lib).optional (flags.threaded) "";
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../.; }