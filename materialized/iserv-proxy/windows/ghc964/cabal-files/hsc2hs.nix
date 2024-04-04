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
    flags = { in-ghc-tree = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "hsc2hs"; version = "0.68.10"; };
      license = "BSD-3-Clause";
      copyright = "2000, Marcin Kowalczyk";
      maintainer = "ghc-devs@haskell.org";
      author = "Marcin Kowalczyk <qrczak@knm.org.pl>";
      homepage = "";
      url = "";
      synopsis = "A preprocessor that helps with writing Haskell bindings to C code";
      description = "The hsc2hs program can be used to automate some parts of the\nprocess of writing Haskell bindings to C code.  It reads an\nalmost-Haskell source file with embedded special constructs, and\noutputs a real Haskell file with these constructs processed, based\non information taken from some C headers.  The extra constructs\nprovide Haskell counterparts of C types, values of C constants,\nincluding sizes of C types, and access to fields of C structs.\n\nFor more details, see the\n<http://downloads.haskell.org/~ghc/master/users-guide/utils.html#writing-haskell-interfaces-to-c-code-hsc2hs hsc2hs section>\nin the GHC User's Guide.";
      buildType = "Simple";
      };
    components = {
      exes = {
        "hsc2hs" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."process" or (errorHandler.buildDepError "process"))
            ] ++ (pkgs.lib).optional (system.isWindows) (hsPkgs."process" or (errorHandler.buildDepError "process"));
          buildable = true;
          };
        };
      tests = {
        "spec" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."test-framework" or (errorHandler.buildDepError "test-framework"))
            (hsPkgs."test-framework-hunit" or (errorHandler.buildDepError "test-framework-hunit"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/hsc2hs-0.68.10.tar.gz";
      sha256 = "6f4e34d788fe2ca7091ee0a10307ee8a7c060a1ba890f2bffad16a7d4d5cef76";
      });
    }) // {
    package-description-override = "cabal-version: >=1.10\nName: hsc2hs\nVersion: 0.68.10\nx-revision: 1\n\nCopyright: 2000, Marcin Kowalczyk\nLicense: BSD3\nLicense-File: LICENSE\nAuthor: Marcin Kowalczyk <qrczak@knm.org.pl>\nMaintainer: ghc-devs@haskell.org\nSynopsis: A preprocessor that helps with writing Haskell bindings to C code\nBug-Reports: https://github.com/haskell/hsc2hs/issues\nDescription:\n    The hsc2hs program can be used to automate some parts of the\n    process of writing Haskell bindings to C code.  It reads an\n    almost-Haskell source file with embedded special constructs, and\n    outputs a real Haskell file with these constructs processed, based\n    on information taken from some C headers.  The extra constructs\n    provide Haskell counterparts of C types, values of C constants,\n    including sizes of C types, and access to fields of C structs.\n    .\n    For more details, see the\n    <http://downloads.haskell.org/~ghc/master/users-guide/utils.html#writing-haskell-interfaces-to-c-code-hsc2hs hsc2hs section>\n    in the GHC User's Guide.\nCategory: Development\nData-Dir: data/\nData-Files: template-hsc.h\nbuild-type: Simple\n\ntested-with:\n  GHC == 9.8.0\n  GHC == 9.6.3\n  GHC == 9.4.7\n  GHC == 9.2.8\n  GHC == 9.0.2\n  GHC == 8.10.7\n  GHC == 8.8.4\n  GHC == 8.6.5\n  GHC == 8.4.4\n  GHC == 8.2.2\n  GHC == 8.0.2\n  GHC == 7.10.3\n  -- GHC == 7.8.4\n  -- GHC == 7.6.3\n  -- GHC == 7.4.2\n  -- GHC == 7.2.2\n  -- GHC == 7.0.4\n\nextra-source-files:\n  changelog.md\n  test/asm/*.s\n\nflag in-ghc-tree\n  description: Are we in a GHC tree?\n  default: False\n  manual: True\n\nsource-repository head\n    Type: git\n    Location: https://github.com/haskell/hsc2hs.git\n\nExecutable hsc2hs\n    Default-Language: Haskell2010\n    Main-Is: Main.hs\n    Hs-Source-Dirs: src/\n    Other-Modules:\n        C\n        Common\n        CrossCodegen\n        DirectCodegen\n        Flags\n        HSCParser\n        ATTParser\n        UtilsCodegen\n        Compat.ResponseFile\n        Compat.TempFile\n        Paths_hsc2hs\n\n    c-sources:\n        cbits/utils.c\n\n    Other-Extensions: CPP, NoMonomorphismRestriction\n\n    Build-Depends: base       >= 4.3.0 && < 4.20,\n                   containers >= 0.4.0 && < 0.8,\n                   directory  >= 1.1.0 && < 1.4,\n                   filepath   >= 1.2.0 && < 1.5,\n                   process    >= 1.1.0 && < 1.7\n\n    if os(windows)\n      -- N.B. Job object support was irreparably broken prior to 1.6.8.\n      -- See https://github.com/haskell/process/issues/167.\n      Build-Depends: process  >= 1.6.8 && < 1.7\n\n    ghc-options:   -Wall\n    if flag(in-ghc-tree)\n       cpp-options: -DIN_GHC_TREE\n\ntest-suite spec\n  main-is:           Spec.hs\n  hs-source-dirs:    src/ test/\n  other-modules:     ATTParser Flags BDD\n  ghc-options:       -Wall -threaded\n  type:              exitcode-stdio-1.0\n  build-depends:     base,\n                     test-framework >=0.8.2.0 && <0.9,\n                     test-framework-hunit >=0.3.0.2 && <0.4,\n                     HUnit >=1.3.1.2 && <1.4 || >=1.6.0.0 && <1.7\n\n  default-language: Haskell2010\n";
    }