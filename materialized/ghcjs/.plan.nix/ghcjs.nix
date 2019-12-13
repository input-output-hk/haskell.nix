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
    flags = {
      compiler-only = false;
      no-wrapper-install = false;
      disable-optimizer = false;
      runtime-assertions = false;
      use-host-template-haskell = false;
      };
    package = {
      specVersion = "2.0";
      identifier = { name = "ghcjs"; version = "8.6.5"; };
      license = "MIT";
      copyright = "Victor Nazarov, Hamish Mackenzie, Luite Stegeman";
      maintainer = "Luite Stegeman <stegeman@gmail.com>";
      author = "Victor Nazarov, Hamish Mackenzie, Luite Stegeman";
      homepage = "";
      url = "";
      synopsis = "Haskell to JavaScript compiler";
      description = "Haskell to JavaScript compiler based on GHC";
      buildType = "Custom";
      isLocal = true;
      setup-depends = [
        (hsPkgs.buildPackages.base or (pkgs.buildPackages.base or (buildToolDepError "base")))
        (hsPkgs.buildPackages.Cabal or (pkgs.buildPackages.Cabal or (buildToolDepError "Cabal")))
        (hsPkgs.buildPackages.containers or (pkgs.buildPackages.containers or (buildToolDepError "containers")))
        (hsPkgs.buildPackages.filepath or (pkgs.buildPackages.filepath or (buildToolDepError "filepath")))
        (hsPkgs.buildPackages.directory or (pkgs.buildPackages.directory or (buildToolDepError "directory")))
        (hsPkgs.buildPackages.process or (pkgs.buildPackages.process or (buildToolDepError "process")))
        (hsPkgs.buildPackages.template-haskell or (pkgs.buildPackages.template-haskell or (buildToolDepError "template-haskell")))
        (hsPkgs.buildPackages.transformers or (pkgs.buildPackages.transformers or (buildToolDepError "transformers")))
        ];
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" ];
      dataDir = "data";
      dataFiles = [ "*.tar" "bin/*.sh" "bin/*.exe.options" ];
      extraSrcFiles = [
        "utils/*.hs"
        "utils/*.sh"
        "include/prim/*.hs-incl"
        "include/prim/*.txt"
        "include/*.h"
        "src-bin/haddock/*.hs"
        "HACKING.markdown"
        "README.markdown"
        "test/LICENSE"
        "test/ghcjs-testsuite.cabal"
        "stack.yaml"
        ];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (buildDepError "base"))
          (hsPkgs."Cabal" or (buildDepError "Cabal"))
          (hsPkgs."ghc-api-ghcjs" or (buildDepError "ghc-api-ghcjs"))
          (hsPkgs."ghcjs-th" or (buildDepError "ghcjs-th"))
          (hsPkgs."ghc-boot" or (buildDepError "ghc-boot"))
          (hsPkgs."directory" or (buildDepError "directory"))
          (hsPkgs."filepath" or (buildDepError "filepath"))
          (hsPkgs."containers" or (buildDepError "containers"))
          (hsPkgs."process" or (buildDepError "process"))
          (hsPkgs."mtl" or (buildDepError "mtl"))
          (hsPkgs."ghc-paths" or (buildDepError "ghc-paths"))
          (hsPkgs."template-haskell" or (buildDepError "template-haskell"))
          (hsPkgs."syb" or (buildDepError "syb"))
          (hsPkgs."bytestring" or (buildDepError "bytestring"))
          (hsPkgs."attoparsec" or (buildDepError "attoparsec"))
          (hsPkgs."aeson" or (buildDepError "aeson"))
          (hsPkgs."text" or (buildDepError "text"))
          (hsPkgs."wl-pprint-text" or (buildDepError "wl-pprint-text"))
          (hsPkgs."lens" or (buildDepError "lens"))
          (hsPkgs."yaml" or (buildDepError "yaml"))
          (hsPkgs."time" or (buildDepError "time"))
          (hsPkgs."transformers" or (buildDepError "transformers"))
          (hsPkgs."split" or (buildDepError "split"))
          (hsPkgs."deepseq" or (buildDepError "deepseq"))
          (hsPkgs."vector" or (buildDepError "vector"))
          (hsPkgs."data-default" or (buildDepError "data-default"))
          (hsPkgs."array" or (buildDepError "array"))
          (hsPkgs."binary" or (buildDepError "binary"))
          (hsPkgs."unordered-containers" or (buildDepError "unordered-containers"))
          (hsPkgs."parallel" or (buildDepError "parallel"))
          (hsPkgs."cryptohash" or (buildDepError "cryptohash"))
          (hsPkgs."hashable" or (buildDepError "hashable"))
          (hsPkgs."optparse-applicative" or (buildDepError "optparse-applicative"))
          (hsPkgs."stringsearch" or (buildDepError "stringsearch"))
          (hsPkgs."base16-bytestring" or (buildDepError "base16-bytestring"))
          (hsPkgs."base64-bytestring" or (buildDepError "base64-bytestring"))
          (hsPkgs."regex-posix" or (buildDepError "regex-posix"))
          (hsPkgs."safe" or (buildDepError "safe"))
          (hsPkgs."parsec" or (buildDepError "parsec"))
          (hsPkgs."haskell-src-exts" or (buildDepError "haskell-src-exts"))
          (hsPkgs."haskell-src-meta" or (buildDepError "haskell-src-meta"))
          ] ++ (if flags.use-host-template-haskell
          then [ (hsPkgs."ghci" or (buildDepError "ghci")) ]
          else [
            (hsPkgs."template-haskell-ghcjs" or (buildDepError "template-haskell-ghcjs"))
            (hsPkgs."ghci-ghcjs" or (buildDepError "ghci-ghcjs"))
            ]);
        buildable = true;
        modules = [
          "Paths_ghcjs"
          "Gen2/Generator"
          "Gen2/Profiling"
          "Gen2/Floater"
          "Gen2/Prim"
          "Gen2/Rts"
          "Gen2/RtsApply"
          "Gen2/RtsTypes"
          "Gen2/RtsAlloc"
          "Gen2/Utils"
          "Gen2/StgAst"
          "Gen2/Optimizer"
          "Gen2/Dataflow"
          "Gen2/Deps"
          "Gen2/Printer"
          "Gen2/Linker"
          "Gen2/Shim"
          "Gen2/Compactor"
          "Gen2/Object"
          "Gen2/Archive"
          "Gen2/ClosureInfo"
          "Gen2/Foreign"
          "Gen2/Sinker"
          "Gen2/TH"
          "Gen2/Base"
          "Gen2/Cache"
          "Gen2/DynamicLinking"
          "Gen2/GHC/Digraph"
          "Gen2/GHC/DsForeign"
          "Compiler/Compat"
          "Compiler/GhcjsHooks"
          "Compiler/GhcjsPlatform"
          "Compiler/Info"
          "Compiler/Plugins"
          "Compiler/Program"
          "Compiler/GhcjsProgram"
          "Compiler/Settings"
          "Compiler/Utils"
          "Compiler/Variants"
          "Compiler/JMacro"
          "Compiler/JMacro/Base"
          "Compiler/JMacro/Lens"
          "Compiler/JMacro/QQ"
          "Compiler/JMacro/ParseTH"
          "Compiler/JMacro/Util"
          "GHCJS"
          ];
        hsSourceDirs = [ "src" ];
        includeDirs = [ "include" ];
        };
      exes = {
        "ghcjs" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."ghcjs" or (buildDepError "ghcjs"))
            ];
          buildable = true;
          hsSourceDirs = [ "src-bin" ];
          mainPath = [ "Main.hs" ];
          };
        "haddock-ghcjs" = {
          depends = [
            (hsPkgs."ghc-api-ghcjs" or (buildDepError "ghc-api-ghcjs"))
            (hsPkgs."haddock-api-ghcjs" or (buildDepError "haddock-api-ghcjs"))
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."process" or (buildDepError "process"))
            (hsPkgs."transformers" or (buildDepError "transformers"))
            (hsPkgs."transformers-compat" or (buildDepError "transformers-compat"))
            (hsPkgs."containers" or (buildDepError "containers"))
            (hsPkgs."directory" or (buildDepError "directory"))
            (hsPkgs."filepath" or (buildDepError "filepath"))
            (hsPkgs."ghcjs" or (buildDepError "ghcjs"))
            ];
          buildable = true;
          modules = [ "ResponseFile" ];
          hsSourceDirs = [ "src-bin" "src-bin/haddock" ];
          mainPath = [
            "Haddock.hs"
            ] ++ (pkgs.lib).optional (system.isWindows) "";
          };
        "hsc2hs-ghcjs" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."ghcjs" or (buildDepError "ghcjs"))
            (hsPkgs."process" or (buildDepError "process"))
            ];
          buildable = true;
          hsSourceDirs = [ "src-bin" ];
          mainPath = [
            "Hsc2Hs.hs"
            ] ++ (pkgs.lib).optional (system.isWindows) "";
          };
        "ghcjs-pkg" = {
          depends = [
            (hsPkgs."ghcjs" or (buildDepError "ghcjs"))
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."directory" or (buildDepError "directory"))
            (hsPkgs."process" or (buildDepError "process"))
            (hsPkgs."filepath" or (buildDepError "filepath"))
            (hsPkgs."containers" or (buildDepError "containers"))
            (hsPkgs."Cabal" or (buildDepError "Cabal"))
            (hsPkgs."binary" or (buildDepError "binary"))
            (hsPkgs."bytestring" or (buildDepError "bytestring"))
            (hsPkgs."ghc-boot" or (buildDepError "ghc-boot"))
            ] ++ (pkgs.lib).optionals (!system.isWindows) [
            (hsPkgs."unix" or (buildDepError "unix"))
            (hsPkgs."terminfo" or (buildDepError "terminfo"))
            ];
          buildable = if flags.compiler-only then false else true;
          cSources = (pkgs.lib).optional (system.isWindows) "cbits/CRT_noglob.c";
          hsSourceDirs = [ "src-bin" ];
          mainPath = (([
            "Pkg.hs"
            ] ++ (pkgs.lib).optional (flags.compiler-only) "") ++ (pkgs.lib).optional (!system.isWindows) "") ++ (pkgs.lib).optional (system.isWindows) "";
          };
        "ghcjs-boot" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."ghcjs" or (buildDepError "ghcjs"))
            (hsPkgs."directory" or (buildDepError "directory"))
            (hsPkgs."Cabal" or (buildDepError "Cabal"))
            (hsPkgs."mtl" or (buildDepError "mtl"))
            (hsPkgs."transformers" or (buildDepError "transformers"))
            (hsPkgs."lens" or (buildDepError "lens"))
            (hsPkgs."unordered-containers" or (buildDepError "unordered-containers"))
            (hsPkgs."vector" or (buildDepError "vector"))
            (hsPkgs."filepath" or (buildDepError "filepath"))
            (hsPkgs."aeson" or (buildDepError "aeson"))
            (hsPkgs."text" or (buildDepError "text"))
            (hsPkgs."bytestring" or (buildDepError "bytestring"))
            (hsPkgs."system-filepath" or (buildDepError "system-filepath"))
            (hsPkgs."shelly" or (buildDepError "shelly"))
            (hsPkgs."system-fileio" or (buildDepError "system-fileio"))
            (hsPkgs."optparse-applicative" or (buildDepError "optparse-applicative"))
            (hsPkgs."tar" or (buildDepError "tar"))
            (hsPkgs."yaml" or (buildDepError "yaml"))
            (hsPkgs."process" or (buildDepError "process"))
            (hsPkgs."time" or (buildDepError "time"))
            (hsPkgs."unix-compat" or (buildDepError "unix-compat"))
            (hsPkgs."executable-path" or (buildDepError "executable-path"))
            ];
          buildable = if flags.compiler-only then false else true;
          hsSourceDirs = [ "src-bin" ];
          mainPath = ([
            "Boot.hs"
            ] ++ (pkgs.lib).optional (flags.compiler-only) "") ++ (pkgs.lib).optional (system.isWindows) "";
          };
        "ghcjs-run" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."directory" or (buildDepError "directory"))
            (hsPkgs."process" or (buildDepError "process"))
            (hsPkgs."filepath" or (buildDepError "filepath"))
            ];
          buildable = if flags.compiler-only then false else true;
          hsSourceDirs = [ "src-bin" ];
          mainPath = ([
            "Run.hs"
            ] ++ (pkgs.lib).optional (flags.compiler-only) "") ++ (pkgs.lib).optional (system.isWindows) "";
          };
        "ghcjs-dumparchive" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."text" or (buildDepError "text"))
            (hsPkgs."ghc-api-ghcjs" or (buildDepError "ghc-api-ghcjs"))
            (hsPkgs."ghcjs" or (buildDepError "ghcjs"))
            (hsPkgs."bytestring" or (buildDepError "bytestring"))
            (hsPkgs."filepath" or (buildDepError "filepath"))
            ];
          buildable = if flags.compiler-only then false else true;
          hsSourceDirs = [ "utils" ];
          mainPath = ([
            "dumpArchive.hs"
            ] ++ (pkgs.lib).optional (flags.compiler-only) "") ++ (pkgs.lib).optional (system.isWindows) "";
          };
        };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."process" or (buildDepError "process"))
            (hsPkgs."network" or (buildDepError "network"))
            (hsPkgs."aeson" or (buildDepError "aeson"))
            (hsPkgs."test-framework" or (buildDepError "test-framework"))
            (hsPkgs."test-framework-hunit" or (buildDepError "test-framework-hunit"))
            (hsPkgs."HUnit" or (buildDepError "HUnit"))
            (hsPkgs."system-filepath" or (buildDepError "system-filepath"))
            (hsPkgs."system-fileio" or (buildDepError "system-fileio"))
            (hsPkgs."text" or (buildDepError "text"))
            (hsPkgs."transformers" or (buildDepError "transformers"))
            (hsPkgs."lens" or (buildDepError "lens"))
            (hsPkgs."random" or (buildDepError "random"))
            (hsPkgs."time" or (buildDepError "time"))
            (hsPkgs."bytestring" or (buildDepError "bytestring"))
            (hsPkgs."deepseq" or (buildDepError "deepseq"))
            (hsPkgs."unordered-containers" or (buildDepError "unordered-containers"))
            (hsPkgs."shelly" or (buildDepError "shelly"))
            (hsPkgs."data-default" or (buildDepError "data-default"))
            (hsPkgs."yaml" or (buildDepError "yaml"))
            (hsPkgs."optparse-applicative" or (buildDepError "optparse-applicative"))
            (hsPkgs."directory" or (buildDepError "directory"))
            (hsPkgs."http-types" or (buildDepError "http-types"))
            (hsPkgs."warp" or (buildDepError "warp"))
            (hsPkgs."wai" or (buildDepError "wai"))
            (hsPkgs."wai-extra" or (buildDepError "wai-extra"))
            (hsPkgs."wai-app-static" or (buildDepError "wai-app-static"))
            (hsPkgs."wai-websockets" or (buildDepError "wai-websockets"))
            (hsPkgs."websockets" or (buildDepError "websockets"))
            (hsPkgs."webdriver" or (buildDepError "webdriver"))
            (hsPkgs."lifted-base" or (buildDepError "lifted-base"))
            ];
          buildable = true;
          modules = [ "Server" "Client" "Types" ];
          hsSourceDirs = [ "test" ];
          mainPath = [ "TestRunner.hs" ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../.; }