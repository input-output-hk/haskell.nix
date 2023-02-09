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
    flags = { lifted = false; build-examples = false; };
    package = {
      specVersion = "1.8";
      identifier = { name = "shelly"; version = "1.8.1"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Greg Weber <greg@gregweber.info>";
      author = "Greg Weber, Petr Rockai";
      homepage = "https://github.com/yesodweb/Shelly.hs";
      url = "";
      synopsis = "shell-like (systems) programming in Haskell";
      description = "Shelly provides convenient systems programming in Haskell,\nsimilar in spirit to POSIX shells. Shelly:\n\n* is aimed at convenience and getting things done rather than\nbeing a demonstration of elegance.\n\n* has detailed and useful error messages\n\n* maintains its own environment, making it thread-safe.\n\n* is modern, using Text and system-filepath/system-fileio\n\nShelly is originally forked from the Shellish package.\n\nSee the shelly-extra package for additional functionality.\n\nAn overview is available in the README: <https://github.com/yesodweb/Shelly.hs/blob/master/README.md>";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."process" or (errorHandler.buildDepError "process"))
          (hsPkgs."unix-compat" or (errorHandler.buildDepError "unix-compat"))
          (hsPkgs."unix" or (errorHandler.buildDepError "unix"))
          (hsPkgs."system-filepath" or (errorHandler.buildDepError "system-filepath"))
          (hsPkgs."system-fileio" or (errorHandler.buildDepError "system-fileio"))
          (hsPkgs."monad-control" or (errorHandler.buildDepError "monad-control"))
          (hsPkgs."lifted-base" or (errorHandler.buildDepError "lifted-base"))
          (hsPkgs."lifted-async" or (errorHandler.buildDepError "lifted-async"))
          (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
          (hsPkgs."enclosed-exceptions" or (errorHandler.buildDepError "enclosed-exceptions"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."async" or (errorHandler.buildDepError "async"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."transformers-base" or (errorHandler.buildDepError "transformers-base"))
          ] ++ [ (hsPkgs."base" or (errorHandler.buildDepError "base")) ];
        buildable = true;
        };
      exes = {
        "drain" = {
          depends = (pkgs.lib).optionals (flags.build-examples) [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."shelly" or (errorHandler.buildDepError "shelly"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            ];
          buildable = if flags.build-examples then true else false;
          };
        "run-handles" = {
          depends = (pkgs.lib).optionals (flags.build-examples) [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."shelly" or (errorHandler.buildDepError "shelly"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            ];
          buildable = if flags.build-examples then true else false;
          };
        "Color" = {
          depends = (pkgs.lib).optionals (flags.build-examples) [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."process" or (errorHandler.buildDepError "process"))
            (hsPkgs."shelly" or (errorHandler.buildDepError "shelly"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            ];
          buildable = if flags.build-examples then true else false;
          };
        };
      tests = {
        "shelly-testsuite" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."async" or (errorHandler.buildDepError "async"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."process" or (errorHandler.buildDepError "process"))
            (hsPkgs."unix-compat" or (errorHandler.buildDepError "unix-compat"))
            (hsPkgs."unix" or (errorHandler.buildDepError "unix"))
            (hsPkgs."system-filepath" or (errorHandler.buildDepError "system-filepath"))
            (hsPkgs."system-fileio" or (errorHandler.buildDepError "system-fileio"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."hspec-contrib" or (errorHandler.buildDepError "hspec-contrib"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."transformers-base" or (errorHandler.buildDepError "transformers-base"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."monad-control" or (errorHandler.buildDepError "monad-control"))
            (hsPkgs."lifted-base" or (errorHandler.buildDepError "lifted-base"))
            (hsPkgs."lifted-async" or (errorHandler.buildDepError "lifted-async"))
            (hsPkgs."enclosed-exceptions" or (errorHandler.buildDepError "enclosed-exceptions"))
            (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/shelly-1.8.1.tar.gz";
      sha256 = "de8814879c7a5e7f1f7f0d9c56c1dfee30d6d63ba1140946e5ed158dd75e6e08";
      });
    }) // {
    package-description-override = "Name:       shelly\n\nVersion:     1.8.1\nx-revision: 1\nSynopsis:    shell-like (systems) programming in Haskell\n\nDescription: Shelly provides convenient systems programming in Haskell,\n             similar in spirit to POSIX shells. Shelly:\n             .\n               * is aimed at convenience and getting things done rather than\n                 being a demonstration of elegance.\n             .\n               * has detailed and useful error messages\n             .\n               * maintains its own environment, making it thread-safe.\n             .\n               * is modern, using Text and system-filepath/system-fileio\n             .\n             Shelly is originally forked from the Shellish package.\n             .\n             See the shelly-extra package for additional functionality.\n             .\n             An overview is available in the README: <https://github.com/yesodweb/Shelly.hs/blob/master/README.md>\n\n\nHomepage:            https://github.com/yesodweb/Shelly.hs\nLicense:             BSD3\nLicense-file:        LICENSE\nAuthor:              Greg Weber, Petr Rockai\nMaintainer:          Greg Weber <greg@gregweber.info>\nCategory:            Development\nBuild-type:          Simple\nCabal-version:       >=1.8\n\n-- for the sdist of the test suite\nextra-source-files: test/src/*.hs\n                    test/examples/*.sh\n                    test/examples/*.hs\n                    test/data/zshrc\n                    test/data/nonascii.txt\n                    test/data/symlinked_dir/hoge_file\n                    test/testall\n                    README.md\n                    ChangeLog.md\n\nLibrary\n  Exposed-modules: Shelly, Shelly.Lifted, Shelly.Pipe, Shelly.Unix\n  other-modules:   Shelly.Base, Shelly.Find, Shelly.Directory\n  hs-source-dirs: src\n  other-extensions: InstanceSigs\n\n  Build-depends:\n    containers                >= 0.4.2.0,\n    time                      >= 1.3 && < 1.9,\n    directory                 >= 1.3.0.0 && < 1.4.0.0,\n    mtl                       >= 2,\n    process                   >= 1.0,\n    unix-compat               < 0.6,\n    unix,\n    system-filepath           >= 0.4.7 && < 0.5,\n    system-fileio             < 0.4,\n    monad-control             >= 0.3.2 && < 1.1,\n    lifted-base,\n    lifted-async,\n    exceptions                >= 0.6,\n    enclosed-exceptions,\n    text, bytestring, async, transformers, transformers-base\n\n  if impl(ghc >= 7.6.1)\n    build-depends:\n        base >= 4.6 && < 5\n  else\n    build-depends:\n      base >= 4 && < 5\n\n  ghc-options: -Wall\n\n  if impl(ghc >= 7.6.1)\n      CPP-Options: -DNO_PRELUDE_CATCH\n\n  extensions:\n    CPP\n\nsource-repository head\n  type:     git\n  location: https://github.com/yesodweb/Shelly.hs\n\nFlag lifted\n   Description: run the tests against Shelly.Lifted\n   Default: False\n\nTest-Suite shelly-testsuite\n  type: exitcode-stdio-1.0\n  hs-source-dirs: src test/src\n  main-is: TestMain.hs\n  other-modules:\n    CopySpec\n    EnvSpec\n    FailureSpec\n    FindSpec\n    Help\n    LiftedSpec\n    MoveSpec\n    ReadFileSpec\n    RmSpec\n    RunSpec\n    SshSpec\n    Shelly\n    Shelly.Base\n    Shelly.Find\n    Shelly.Lifted\n    TestInit\n    WhichSpec\n    WriteSpec\n\n  ghc-options: -O2 -Wall -fwarn-tabs -funbox-strict-fields -threaded\n               -fno-warn-unused-do-bind -fno-warn-type-defaults\n\n\n  extensions: OverloadedStrings, ExtendedDefaultRules\n\n  if flag(lifted)\n     cpp-options: -DLIFTED\n\n  build-depends:\n    base                      >= 4.6,\n    text                      >= 0.11,\n    async,\n    bytestring                >= 0.10,\n    containers                >= 0.5.0.0,\n    directory                 >= 1.3.0.0 && < 1.4.0.0,\n    process                   >= 1.1.0,\n    unix-compat               < 0.6,\n    unix,\n    system-filepath           >= 0.4.7 && < 0.5,\n    system-fileio             < 0.4,\n    time                      >= 1.3 && < 1.9,\n    mtl                       >= 2,\n    HUnit                     >= 1.2,\n    hspec                     >= 2.0,\n    hspec-contrib,\n    transformers,\n    transformers-base,\n    filepath,\n    monad-control,\n    lifted-base,\n    lifted-async,\n    enclosed-exceptions,\n    exceptions\n\n  extensions:\n    CPP\n\nFlag build-examples\n   Description: build some example programs\n   Default: False\n   Manual: True\n\n-- demonstarated that command output in Shellish was not shown until after the command finished\n-- not necessary anymore\nExecutable drain\n  hs-source-dirs: test/examples\n  main-is: drain.hs\n  if flag(build-examples)\n    buildable: True\n\n    build-depends: base                      >= 4.6\n                 , shelly\n                 , text\n\n    extensions:\n      CPP\n  else\n    buildable: False\n\nExecutable run-handles\n  hs-source-dirs: test/examples\n  main-is: run-handles.hs\n  if flag(build-examples)\n    buildable: True\n\n    build-depends: base                      >= 4.6\n                 , shelly\n                 , text\n\n    extensions:\n      CPP\n  else\n    buildable: False\n\nExecutable Color\n  hs-source-dirs: test/examples\n  main-is: color.hs\n  if flag(build-examples)\n    buildable: True\n\n    build-depends: base                      >= 4.6\n                 , process\n                 , shelly\n                 , text\n  else\n    buildable: False\n";
    }