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
      specVersion = "1.8";
      identifier = { name = "stylish-haskell"; version = "0.11.0.0"; };
      license = "BSD-3-Clause";
      copyright = "2012 Jasper Van der Jeugt";
      maintainer = "Jasper Van der Jeugt <m@jaspervdj.be>";
      author = "Jasper Van der Jeugt <m@jaspervdj.be>";
      homepage = "https://github.com/jaspervdj/stylish-haskell";
      url = "";
      synopsis = "Haskell code prettifier";
      description = "A Haskell code prettifier. For more information, see:\n\n<https://github.com/jaspervdj/stylish-haskell/blob/master/README.markdown>";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" ];
      dataDir = "";
      dataFiles = [];
      extraSrcFiles = [
        "CHANGELOG"
        "README.markdown"
        "data/stylish-haskell.yaml"
        ];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."aeson" or (buildDepError "aeson"))
          (hsPkgs."base" or (buildDepError "base"))
          (hsPkgs."bytestring" or (buildDepError "bytestring"))
          (hsPkgs."Cabal" or (buildDepError "Cabal"))
          (hsPkgs."containers" or (buildDepError "containers"))
          (hsPkgs."directory" or (buildDepError "directory"))
          (hsPkgs."filepath" or (buildDepError "filepath"))
          (hsPkgs."file-embed" or (buildDepError "file-embed"))
          (hsPkgs."haskell-src-exts" or (buildDepError "haskell-src-exts"))
          (hsPkgs."mtl" or (buildDepError "mtl"))
          (hsPkgs."semigroups" or (buildDepError "semigroups"))
          (hsPkgs."syb" or (buildDepError "syb"))
          (hsPkgs."text" or (buildDepError "text"))
          (hsPkgs."HsYAML-aeson" or (buildDepError "HsYAML-aeson"))
          (hsPkgs."HsYAML" or (buildDepError "HsYAML"))
          ];
        buildable = true;
        modules = [
          "Language/Haskell/Stylish/Align"
          "Language/Haskell/Stylish/Block"
          "Language/Haskell/Stylish/Config"
          "Language/Haskell/Stylish/Config/Cabal"
          "Language/Haskell/Stylish/Config/Internal"
          "Language/Haskell/Stylish/Editor"
          "Language/Haskell/Stylish/Parse"
          "Language/Haskell/Stylish/Step"
          "Language/Haskell/Stylish/Util"
          "Language/Haskell/Stylish/Verbose"
          "Paths_stylish_haskell"
          "Language/Haskell/Stylish"
          "Language/Haskell/Stylish/Step/Data"
          "Language/Haskell/Stylish/Step/Imports"
          "Language/Haskell/Stylish/Step/LanguagePragmas"
          "Language/Haskell/Stylish/Step/SimpleAlign"
          "Language/Haskell/Stylish/Step/Squash"
          "Language/Haskell/Stylish/Step/Tabs"
          "Language/Haskell/Stylish/Step/TrailingWhitespace"
          "Language/Haskell/Stylish/Step/UnicodeSyntax"
          ];
        hsSourceDirs = [ "lib" ];
        };
      exes = {
        "stylish-haskell" = {
          depends = [
            (hsPkgs."stylish-haskell" or (buildDepError "stylish-haskell"))
            (hsPkgs."strict" or (buildDepError "strict"))
            (hsPkgs."optparse-applicative" or (buildDepError "optparse-applicative"))
            (hsPkgs."aeson" or (buildDepError "aeson"))
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."bytestring" or (buildDepError "bytestring"))
            (hsPkgs."Cabal" or (buildDepError "Cabal"))
            (hsPkgs."containers" or (buildDepError "containers"))
            (hsPkgs."directory" or (buildDepError "directory"))
            (hsPkgs."filepath" or (buildDepError "filepath"))
            (hsPkgs."file-embed" or (buildDepError "file-embed"))
            (hsPkgs."haskell-src-exts" or (buildDepError "haskell-src-exts"))
            (hsPkgs."mtl" or (buildDepError "mtl"))
            (hsPkgs."syb" or (buildDepError "syb"))
            (hsPkgs."HsYAML-aeson" or (buildDepError "HsYAML-aeson"))
            (hsPkgs."HsYAML" or (buildDepError "HsYAML"))
            ];
          buildable = true;
          hsSourceDirs = [ "src" ];
          mainPath = [ "Main.hs" ];
          };
        };
      tests = {
        "stylish-haskell-tests" = {
          depends = [
            (hsPkgs."HUnit" or (buildDepError "HUnit"))
            (hsPkgs."test-framework" or (buildDepError "test-framework"))
            (hsPkgs."test-framework-hunit" or (buildDepError "test-framework-hunit"))
            (hsPkgs."random" or (buildDepError "random"))
            (hsPkgs."aeson" or (buildDepError "aeson"))
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."bytestring" or (buildDepError "bytestring"))
            (hsPkgs."Cabal" or (buildDepError "Cabal"))
            (hsPkgs."containers" or (buildDepError "containers"))
            (hsPkgs."directory" or (buildDepError "directory"))
            (hsPkgs."filepath" or (buildDepError "filepath"))
            (hsPkgs."file-embed" or (buildDepError "file-embed"))
            (hsPkgs."haskell-src-exts" or (buildDepError "haskell-src-exts"))
            (hsPkgs."mtl" or (buildDepError "mtl"))
            (hsPkgs."syb" or (buildDepError "syb"))
            (hsPkgs."text" or (buildDepError "text"))
            (hsPkgs."HsYAML-aeson" or (buildDepError "HsYAML-aeson"))
            (hsPkgs."HsYAML" or (buildDepError "HsYAML"))
            ];
          buildable = true;
          modules = [
            "Language/Haskell/Stylish"
            "Language/Haskell/Stylish/Align"
            "Language/Haskell/Stylish/Block"
            "Language/Haskell/Stylish/Config"
            "Language/Haskell/Stylish/Config/Cabal"
            "Language/Haskell/Stylish/Config/Internal"
            "Language/Haskell/Stylish/Config/Tests"
            "Language/Haskell/Stylish/Editor"
            "Language/Haskell/Stylish/Parse"
            "Language/Haskell/Stylish/Parse/Tests"
            "Language/Haskell/Stylish/Step"
            "Language/Haskell/Stylish/Step/Imports"
            "Language/Haskell/Stylish/Step/Imports/Tests"
            "Language/Haskell/Stylish/Step/Data"
            "Language/Haskell/Stylish/Step/Data/Tests"
            "Language/Haskell/Stylish/Step/LanguagePragmas"
            "Language/Haskell/Stylish/Step/LanguagePragmas/Tests"
            "Language/Haskell/Stylish/Step/SimpleAlign"
            "Language/Haskell/Stylish/Step/SimpleAlign/Tests"
            "Language/Haskell/Stylish/Step/Squash"
            "Language/Haskell/Stylish/Step/Squash/Tests"
            "Language/Haskell/Stylish/Step/Tabs"
            "Language/Haskell/Stylish/Step/Tabs/Tests"
            "Language/Haskell/Stylish/Step/TrailingWhitespace"
            "Language/Haskell/Stylish/Step/TrailingWhitespace/Tests"
            "Language/Haskell/Stylish/Step/UnicodeSyntax"
            "Language/Haskell/Stylish/Step/UnicodeSyntax/Tests"
            "Language/Haskell/Stylish/Tests"
            "Language/Haskell/Stylish/Tests/Util"
            "Language/Haskell/Stylish/Util"
            "Language/Haskell/Stylish/Verbose"
            "Paths_stylish_haskell"
            ];
          hsSourceDirs = [ "tests" "lib" ];
          mainPath = [ "TestSuite.hs" ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../.; }