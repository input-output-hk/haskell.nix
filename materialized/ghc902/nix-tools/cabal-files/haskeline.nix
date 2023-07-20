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
    flags = { terminfo = true; examples = true; };
    package = {
      specVersion = "1.10";
      identifier = { name = "haskeline"; version = "0.8.2.1"; };
      license = "BSD-3-Clause";
      copyright = "(c) Judah Jacobson";
      maintainer = "Judah Jacobson <judah.jacobson@gmail.com>";
      author = "Judah Jacobson";
      homepage = "https://github.com/judah/haskeline";
      url = "";
      synopsis = "A command-line interface for user input, written in Haskell.";
      description = "Haskeline provides a user interface for line input in command-line\nprograms.  This library is similar in purpose to readline, but since\nit is written in Haskell it is (hopefully) more easily used in other\nHaskell programs.\n\nHaskeline runs both on POSIX-compatible systems and on Windows.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."process" or (errorHandler.buildDepError "process"))
          (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
          (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
          ] ++ (if system.isWindows
          then [ (hsPkgs."Win32" or (errorHandler.buildDepError "Win32")) ]
          else [
            (hsPkgs."unix" or (errorHandler.buildDepError "unix"))
            ] ++ (pkgs.lib).optional (flags.terminfo) (hsPkgs."terminfo" or (errorHandler.buildDepError "terminfo")));
        buildable = true;
        };
      exes = {
        "haskeline-examples-Test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."haskeline" or (errorHandler.buildDepError "haskeline"))
            ];
          buildable = if !flags.examples then false else true;
          };
        };
      tests = {
        "haskeline-tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."process" or (errorHandler.buildDepError "process"))
            (hsPkgs."unix" or (errorHandler.buildDepError "unix"))
            ];
          build-tools = [
            (hsPkgs.buildPackages.haskeline.components.exes.haskeline-examples-Test or (pkgs.buildPackages.haskeline-examples-Test or (errorHandler.buildToolDepError "haskeline:haskeline-examples-Test")))
            ];
          buildable = (if system.isWindows
            then false
            else true) && (if !flags.examples then false else true);
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/haskeline-0.8.2.1.tar.gz";
      sha256 = "a08729f2d6a0a498052e508ebe083f56f1c68e62659f3bc92bf7d3d320cd40ff";
      });
    }) // {
    package-description-override = "Name:           haskeline\nCabal-Version:  >=1.10\nVersion:        0.8.2.1\nCategory:       User Interfaces\nLicense:        BSD3\nLicense-File:   LICENSE\nCopyright:      (c) Judah Jacobson\nAuthor:         Judah Jacobson\nMaintainer:     Judah Jacobson <judah.jacobson@gmail.com>\nSynopsis:       A command-line interface for user input, written in Haskell.\nDescription:\n                Haskeline provides a user interface for line input in command-line\n                programs.  This library is similar in purpose to readline, but since\n                it is written in Haskell it is (hopefully) more easily used in other\n                Haskell programs.\n                .\n                Haskeline runs both on POSIX-compatible systems and on Windows.\nHomepage:       https://github.com/judah/haskeline\nBug-Reports:    https://github.com/judah/haskeline/issues\nStability:      Stable\nBuild-Type:     Simple\nextra-source-files: examples/Test.hs Changelog includes/*.h\n\nsource-repository head\n    type: git\n    location: git://github.com/judah/haskeline.git\n\n-- There are three main advantages to the terminfo backend over the portable,\n-- \"dumb\" alternative.  First, it enables more efficient control sequences\n-- when redrawing the input.  Second, and more importantly, it enables us\n-- to draw on multiple lines, so we can wrap long input strings.  And third,\n-- the backend adds some extra key sequences such as forwards delete.\n--\n-- (The \"dumb\" terminal also allows editing of long input strings, but is\n-- restricted to only one line and thus only shows part of the input at once.)\nflag terminfo\n    Description: Use the terminfo package for POSIX consoles.\n    Default: True\n    Manual: True\n\n-- Help the GHC build by making it possible to disable the extra binary.\n-- TODO: Make GHC handle packages with both a library and an executable.\nflag examples\n    Description: Enable executable components used for tests.\n    Default: True\n    Manual: True\n\nLibrary\n    -- We require ghc>=7.4.1 (base>=4.5) to use the base library encodings, even\n    -- though it was implemented in earlier releases, due to GHC bug #5436 which\n    -- wasn't fixed until 7.4.1\n    Build-depends: base >=4.9 && < 4.19, containers>=0.4 && < 0.7,\n                   directory>=1.1 && < 1.4, bytestring>=0.9 && < 0.12,\n                   filepath >= 1.2 && < 1.5, transformers >= 0.2 && < 0.7,\n                   process >= 1.0 && < 1.7, stm >= 2.4 && < 2.6,\n                   exceptions == 0.10.*\n    Default-Language: Haskell98\n    Default-Extensions:\n                ForeignFunctionInterface, Rank2Types, FlexibleInstances,\n                TypeSynonymInstances\n                FlexibleContexts, ExistentialQuantification\n                ScopedTypeVariables, GeneralizedNewtypeDeriving\n                StandaloneDeriving\n                MultiParamTypeClasses,\n                UndecidableInstances\n                ScopedTypeVariables, CPP, DeriveDataTypeable,\n                PatternGuards\n    Exposed-Modules:\n                System.Console.Haskeline\n                System.Console.Haskeline.Completion\n                System.Console.Haskeline.History\n                System.Console.Haskeline.IO\n                System.Console.Haskeline.Internal\n    Other-Modules:\n                System.Console.Haskeline.Backend\n                System.Console.Haskeline.Backend.WCWidth\n                System.Console.Haskeline.Command\n                System.Console.Haskeline.Command.Completion\n                System.Console.Haskeline.Command.History\n                System.Console.Haskeline.Command.KillRing\n                System.Console.Haskeline.Directory\n                System.Console.Haskeline.Emacs\n                System.Console.Haskeline.InputT\n                System.Console.Haskeline.Key\n                System.Console.Haskeline.LineState\n                System.Console.Haskeline.Monads\n                System.Console.Haskeline.Prefs\n                System.Console.Haskeline.Recover\n                System.Console.Haskeline.RunCommand\n                System.Console.Haskeline.Term\n                System.Console.Haskeline.Command.Undo\n                System.Console.Haskeline.Vi\n    include-dirs: includes\n    c-sources: cbits/h_wcwidth.c\n\n    if os(windows) {\n        Build-depends: Win32>=2.0\n        Other-modules: System.Console.Haskeline.Backend.Win32\n                       System.Console.Haskeline.Backend.Win32.Echo\n        c-sources: cbits/win_console.c\n        includes: win_console.h, windows_cconv.h\n        install-includes: win_console.h\n        cpp-options: -DMINGW\n    } else {\n        Build-depends: unix>=2.0 && < 2.9\n        Other-modules:\n                System.Console.Haskeline.Backend.Posix\n                System.Console.Haskeline.Backend.Posix.Encoder\n                System.Console.Haskeline.Backend.DumbTerm\n        if flag(terminfo) {\n            Build-depends: terminfo>=0.3.1.3 && <0.5\n            Other-modules: System.Console.Haskeline.Backend.Terminfo\n            cpp-options: -DTERMINFO\n        }\n        if os(solaris) {\n            cpp-options: -DUSE_TERMIOS_H\n        }\n    }\n\n    ghc-options: -Wall -Wcompat\n\ntest-suite haskeline-tests\n    type: exitcode-stdio-1.0\n    hs-source-dirs: tests\n    Default-Language: Haskell98\n\n    if os(windows) {\n        buildable: False\n    }\n    if !flag(examples) {\n        buildable: False\n    }\n    Main-Is:    Unit.hs\n    Build-depends: base, containers, text, bytestring, HUnit, process, unix\n    Other-Modules: RunTTY, Pty\n    build-tool-depends: haskeline:haskeline-examples-Test\n\n-- The following program is used by unit tests in `tests` executable\nExecutable haskeline-examples-Test\n    if !flag(examples) {\n        buildable: False\n    }\n    Build-depends: base, containers, haskeline\n    Default-Language: Haskell2010\n    hs-source-dirs: examples\n    Main-Is: Test.hs\n";
    }