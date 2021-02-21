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
        (hsPkgs.buildPackages.base or (pkgs.buildPackages.base or (errorHandler.setupDepError "base")))
        (hsPkgs.buildPackages.Cabal or (pkgs.buildPackages.Cabal or (errorHandler.setupDepError "Cabal")))
        (hsPkgs.buildPackages.containers or (pkgs.buildPackages.containers or (errorHandler.setupDepError "containers")))
        (hsPkgs.buildPackages.filepath or (pkgs.buildPackages.filepath or (errorHandler.setupDepError "filepath")))
        (hsPkgs.buildPackages.directory or (pkgs.buildPackages.directory or (errorHandler.setupDepError "directory")))
        (hsPkgs.buildPackages.process or (pkgs.buildPackages.process or (errorHandler.setupDepError "process")))
        (hsPkgs.buildPackages.template-haskell or (pkgs.buildPackages.template-haskell or (errorHandler.setupDepError "template-haskell")))
        (hsPkgs.buildPackages.transformers or (pkgs.buildPackages.transformers or (errorHandler.setupDepError "transformers")))
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
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."Cabal" or (errorHandler.buildDepError "Cabal"))
          (hsPkgs."ghc-api-ghcjs" or (errorHandler.buildDepError "ghc-api-ghcjs"))
          (hsPkgs."ghcjs-th" or (errorHandler.buildDepError "ghcjs-th"))
          (hsPkgs."ghc-boot" or (errorHandler.buildDepError "ghc-boot"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."process" or (errorHandler.buildDepError "process"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."ghc-paths" or (errorHandler.buildDepError "ghc-paths"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."syb" or (errorHandler.buildDepError "syb"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."attoparsec" or (errorHandler.buildDepError "attoparsec"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."wl-pprint-text" or (errorHandler.buildDepError "wl-pprint-text"))
          (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
          (hsPkgs."yaml" or (errorHandler.buildDepError "yaml"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."split" or (errorHandler.buildDepError "split"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          (hsPkgs."data-default" or (errorHandler.buildDepError "data-default"))
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          (hsPkgs."parallel" or (errorHandler.buildDepError "parallel"))
          (hsPkgs."cryptohash" or (errorHandler.buildDepError "cryptohash"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
          (hsPkgs."stringsearch" or (errorHandler.buildDepError "stringsearch"))
          (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
          (hsPkgs."base64-bytestring" or (errorHandler.buildDepError "base64-bytestring"))
          (hsPkgs."regex-posix" or (errorHandler.buildDepError "regex-posix"))
          (hsPkgs."safe" or (errorHandler.buildDepError "safe"))
          (hsPkgs."parsec" or (errorHandler.buildDepError "parsec"))
          (hsPkgs."haskell-src-exts" or (errorHandler.buildDepError "haskell-src-exts"))
          (hsPkgs."haskell-src-meta" or (errorHandler.buildDepError "haskell-src-meta"))
          ] ++ (if flags.use-host-template-haskell
          then [ (hsPkgs."ghci" or (errorHandler.buildDepError "ghci")) ]
          else [
            (hsPkgs."template-haskell-ghcjs" or (errorHandler.buildDepError "template-haskell-ghcjs"))
            (hsPkgs."ghci-ghcjs" or (errorHandler.buildDepError "ghci-ghcjs"))
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
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."ghcjs" or (errorHandler.buildDepError "ghcjs"))
            ];
          buildable = true;
          hsSourceDirs = [ "src-bin" ];
          mainPath = [ "Main.hs" ];
          };
        "haddock-ghcjs" = {
          depends = [
            (hsPkgs."ghc-api-ghcjs" or (errorHandler.buildDepError "ghc-api-ghcjs"))
            (hsPkgs."haddock-api-ghcjs" or (errorHandler.buildDepError "haddock-api-ghcjs"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."process" or (errorHandler.buildDepError "process"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."transformers-compat" or (errorHandler.buildDepError "transformers-compat"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."ghcjs" or (errorHandler.buildDepError "ghcjs"))
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
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."ghcjs" or (errorHandler.buildDepError "ghcjs"))
            (hsPkgs."process" or (errorHandler.buildDepError "process"))
            ];
          buildable = true;
          hsSourceDirs = [ "src-bin" ];
          mainPath = [
            "Hsc2Hs.hs"
            ] ++ (pkgs.lib).optional (system.isWindows) "";
          };
        "ghcjs-pkg" = {
          depends = [
            (hsPkgs."ghcjs" or (errorHandler.buildDepError "ghcjs"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."process" or (errorHandler.buildDepError "process"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."Cabal" or (errorHandler.buildDepError "Cabal"))
            (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."ghc-boot" or (errorHandler.buildDepError "ghc-boot"))
            ] ++ (pkgs.lib).optionals (!system.isWindows) [
            (hsPkgs."unix" or (errorHandler.buildDepError "unix"))
            (hsPkgs."terminfo" or (errorHandler.buildDepError "terminfo"))
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
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."ghcjs" or (errorHandler.buildDepError "ghcjs"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."Cabal" or (errorHandler.buildDepError "Cabal"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."system-filepath" or (errorHandler.buildDepError "system-filepath"))
            (hsPkgs."shelly" or (errorHandler.buildDepError "shelly"))
            (hsPkgs."system-fileio" or (errorHandler.buildDepError "system-fileio"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."tar" or (errorHandler.buildDepError "tar"))
            (hsPkgs."yaml" or (errorHandler.buildDepError "yaml"))
            (hsPkgs."process" or (errorHandler.buildDepError "process"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."unix-compat" or (errorHandler.buildDepError "unix-compat"))
            (hsPkgs."executable-path" or (errorHandler.buildDepError "executable-path"))
            ];
          buildable = if flags.compiler-only then false else true;
          hsSourceDirs = [ "src-bin" ];
          mainPath = ([
            "Boot.hs"
            ] ++ (pkgs.lib).optional (flags.compiler-only) "") ++ (pkgs.lib).optional (system.isWindows) "";
          };
        "ghcjs-run" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."process" or (errorHandler.buildDepError "process"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            ];
          buildable = if flags.compiler-only then false else true;
          hsSourceDirs = [ "src-bin" ];
          mainPath = ([
            "Run.hs"
            ] ++ (pkgs.lib).optional (flags.compiler-only) "") ++ (pkgs.lib).optional (system.isWindows) "";
          };
        "ghcjs-dumparchive" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."ghc-api-ghcjs" or (errorHandler.buildDepError "ghc-api-ghcjs"))
            (hsPkgs."ghcjs" or (errorHandler.buildDepError "ghcjs"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
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
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."process" or (errorHandler.buildDepError "process"))
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."test-framework" or (errorHandler.buildDepError "test-framework"))
            (hsPkgs."test-framework-hunit" or (errorHandler.buildDepError "test-framework-hunit"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."system-filepath" or (errorHandler.buildDepError "system-filepath"))
            (hsPkgs."system-fileio" or (errorHandler.buildDepError "system-fileio"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            (hsPkgs."shelly" or (errorHandler.buildDepError "shelly"))
            (hsPkgs."data-default" or (errorHandler.buildDepError "data-default"))
            (hsPkgs."yaml" or (errorHandler.buildDepError "yaml"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."http-types" or (errorHandler.buildDepError "http-types"))
            (hsPkgs."warp" or (errorHandler.buildDepError "warp"))
            (hsPkgs."wai" or (errorHandler.buildDepError "wai"))
            (hsPkgs."wai-extra" or (errorHandler.buildDepError "wai-extra"))
            (hsPkgs."wai-app-static" or (errorHandler.buildDepError "wai-app-static"))
            (hsPkgs."wai-websockets" or (errorHandler.buildDepError "wai-websockets"))
            (hsPkgs."websockets" or (errorHandler.buildDepError "websockets"))
            (hsPkgs."webdriver" or (errorHandler.buildDepError "webdriver"))
            (hsPkgs."lifted-base" or (errorHandler.buildDepError "lifted-base"))
            ];
          buildable = true;
          modules = [ "Server" "Client" "Types" ];
          hsSourceDirs = [ "test" ];
          mainPath = [ "TestRunner.hs" ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../.; }