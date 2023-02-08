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
      identifier = { name = "haskeline"; version = "0.8.2"; };
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
      url = "http://hackage.haskell.org/package/haskeline-0.8.2.tar.gz";
      sha256 = "ec9d155c5aff7489c2553e57130ed66f89857188c7a06108900c8d1066fc27df";
      });
    }) // {
    package-description-override = "Name:           haskeline\r\nCabal-Version:  >=1.10\r\nVersion:        0.8.2\r\nx-revision: 2\r\nCategory:       User Interfaces\r\nLicense:        BSD3\r\nLicense-File:   LICENSE\r\nCopyright:      (c) Judah Jacobson\r\nAuthor:         Judah Jacobson\r\nMaintainer:     Judah Jacobson <judah.jacobson@gmail.com>\r\nSynopsis:       A command-line interface for user input, written in Haskell.\r\nDescription:\r\n                Haskeline provides a user interface for line input in command-line\r\n                programs.  This library is similar in purpose to readline, but since\r\n                it is written in Haskell it is (hopefully) more easily used in other\r\n                Haskell programs.\r\n                .\r\n                Haskeline runs both on POSIX-compatible systems and on Windows.\r\nHomepage:       https://github.com/judah/haskeline\r\nBug-Reports:    https://github.com/judah/haskeline/issues\r\nStability:      Stable\r\nBuild-Type:     Simple\r\nextra-source-files: examples/Test.hs Changelog includes/*.h\r\n\r\nsource-repository head\r\n    type: git\r\n    location: git://github.com/judah/haskeline.git\r\n\r\n-- There are three main advantages to the terminfo backend over the portable,\r\n-- \"dumb\" alternative.  First, it enables more efficient control sequences\r\n-- when redrawing the input.  Second, and more importantly, it enables us\r\n-- to draw on multiple lines, so we can wrap long input strings.  And third,\r\n-- the backend adds some extra key sequences such as forwards delete.\r\n--\r\n-- (The \"dumb\" terminal also allows editing of long input strings, but is\r\n-- restricted to only one line and thus only shows part of the input at once.)\r\nflag terminfo\r\n    Description: Use the terminfo package for POSIX consoles.\r\n    Default: True\r\n    Manual: True\r\n\r\n-- Help the GHC build by making it possible to disable the extra binary.\r\n-- TODO: Make GHC handle packages with both a library and an executable.\r\nflag examples\r\n    Description: Enable executable components used for tests.\r\n    Default: True\r\n    Manual: True\r\n\r\nLibrary\r\n    -- We require ghc>=7.4.1 (base>=4.5) to use the base library encodings, even\r\n    -- though it was implemented in earlier releases, due to GHC bug #5436 which\r\n    -- wasn't fixed until 7.4.1\r\n    Build-depends: base >=4.9 && < 4.18, containers>=0.4 && < 0.7,\r\n                   directory>=1.1 && < 1.4, bytestring>=0.9 && < 0.12,\r\n                   filepath >= 1.2 && < 1.5, transformers >= 0.2 && < 0.6,\r\n                   process >= 1.0 && < 1.7, stm >= 2.4 && < 2.6,\r\n                   exceptions == 0.10.*\r\n    Default-Language: Haskell98\r\n    Default-Extensions:\r\n                ForeignFunctionInterface, Rank2Types, FlexibleInstances,\r\n                TypeSynonymInstances\r\n                FlexibleContexts, ExistentialQuantification\r\n                ScopedTypeVariables, GeneralizedNewtypeDeriving\r\n                StandaloneDeriving\r\n                MultiParamTypeClasses,\r\n                UndecidableInstances\r\n                ScopedTypeVariables, CPP, DeriveDataTypeable,\r\n                PatternGuards\r\n    Exposed-Modules:\r\n                System.Console.Haskeline\r\n                System.Console.Haskeline.Completion\r\n                System.Console.Haskeline.History\r\n                System.Console.Haskeline.IO\r\n                System.Console.Haskeline.Internal\r\n    Other-Modules:\r\n                System.Console.Haskeline.Backend\r\n                System.Console.Haskeline.Backend.WCWidth\r\n                System.Console.Haskeline.Command\r\n                System.Console.Haskeline.Command.Completion\r\n                System.Console.Haskeline.Command.History\r\n                System.Console.Haskeline.Command.KillRing\r\n                System.Console.Haskeline.Directory\r\n                System.Console.Haskeline.Emacs\r\n                System.Console.Haskeline.InputT\r\n                System.Console.Haskeline.Key\r\n                System.Console.Haskeline.LineState\r\n                System.Console.Haskeline.Monads\r\n                System.Console.Haskeline.Prefs\r\n                System.Console.Haskeline.Recover\r\n                System.Console.Haskeline.RunCommand\r\n                System.Console.Haskeline.Term\r\n                System.Console.Haskeline.Command.Undo\r\n                System.Console.Haskeline.Vi\r\n    include-dirs: includes\r\n    c-sources: cbits/h_wcwidth.c\r\n\r\n    if os(windows) {\r\n        Build-depends: Win32>=2.1 && <2.10 || >=2.12\r\n        Other-modules: System.Console.Haskeline.Backend.Win32\r\n                       System.Console.Haskeline.Backend.Win32.Echo\r\n        c-sources: cbits/win_console.c\r\n        includes: win_console.h, windows_cconv.h\r\n        install-includes: win_console.h\r\n        cpp-options: -DMINGW\r\n    } else {\r\n        Build-depends: unix>=2.0 && < 2.9\r\n        Other-modules:\r\n                System.Console.Haskeline.Backend.Posix\r\n                System.Console.Haskeline.Backend.Posix.Encoder\r\n                System.Console.Haskeline.Backend.DumbTerm\r\n        if flag(terminfo) {\r\n            Build-depends: terminfo>=0.3.1.3 && <0.5\r\n            Other-modules: System.Console.Haskeline.Backend.Terminfo\r\n            cpp-options: -DTERMINFO\r\n        }\r\n        if os(solaris) {\r\n            cpp-options: -DUSE_TERMIOS_H\r\n        }\r\n    }\r\n\r\n    ghc-options: -Wall -Wcompat\r\n\r\ntest-suite haskeline-tests\r\n    type: exitcode-stdio-1.0\r\n    hs-source-dirs: tests\r\n    Default-Language: Haskell98\r\n\r\n    if os(windows) {\r\n        buildable: False\r\n    }\r\n    if !flag(examples) {\r\n        buildable: False\r\n    }\r\n    Main-Is:    Unit.hs\r\n    Build-depends: base, containers, text, bytestring, HUnit, process, unix\r\n    Other-Modules: RunTTY, Pty\r\n    build-tool-depends: haskeline:haskeline-examples-Test\r\n\r\n-- The following program is used by unit tests in `tests` executable\r\nExecutable haskeline-examples-Test\r\n    if !flag(examples) {\r\n        buildable: False\r\n    }\r\n    Build-depends: base, containers, haskeline\r\n    Default-Language: Haskell2010\r\n    hs-source-dirs: examples\r\n    Main-Is: Test.hs\r\n";
    }