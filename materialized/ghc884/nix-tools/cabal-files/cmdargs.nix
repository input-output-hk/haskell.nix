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
    flags = { testprog = false; quotation = true; };
    package = {
      specVersion = "1.18";
      identifier = { name = "cmdargs"; version = "0.10.21"; };
      license = "BSD-3-Clause";
      copyright = "Neil Mitchell 2009-2021";
      maintainer = "Neil Mitchell <ndmitchell@gmail.com>";
      author = "Neil Mitchell <ndmitchell@gmail.com>";
      homepage = "https://github.com/ndmitchell/cmdargs#readme";
      url = "";
      synopsis = "Command line argument processing";
      description = "This library provides an easy way to define command line parsers. Most users\nwill want to use the \"System.Console.CmdArgs.Implicit\" module, whose\ndocumentation contains an example.\n\n* \"System.Console.CmdArgs.Explicit\" provides a way to write command line\nparsers for both single mode programs (most programs) and multiple\nmode programs (e.g. darcs or cabal). Parsers are defined by constructing\na data structure.\n\n* \"System.Console.CmdArgs.Implicit\" provides a way to concisely define\ncommand line parsers, up to three times shorter than getopt. These parsers\nare translated into the Explicit data type.\n\n* \"System.Console.CmdArgs.GetOpt\" provides a wrapper allowing compatiblity\nwith existing getopt parsers, mapping to the Explicit data type.\n\nFor a general reference on what command line flags are commonly used,\nsee <http://www.faqs.org/docs/artu/ch10s05.html>.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = ([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."process" or (errorHandler.buildDepError "process"))
          ] ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).lt "8.0") (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))) ++ (pkgs.lib).optional (flags.quotation) (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"));
        buildable = true;
        };
      exes = {
        "cmdargs" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."process" or (errorHandler.buildDepError "process"))
            (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
            ];
          buildable = if flags.testprog && flags.quotation then true else false;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/cmdargs-0.10.21.tar.gz";
      sha256 = "f7d8ea5c4e6af368d9b5d2eb994fc29235406fbe91916a6dc63bd883025eca75";
      });
    }) // {
    package-description-override = "cabal-version:      >= 1.18\nbuild-type:         Simple\nname:               cmdargs\nversion:            0.10.21\nlicense:            BSD3\nlicense-file:       LICENSE\ncategory:           Console\nauthor:             Neil Mitchell <ndmitchell@gmail.com>\nmaintainer:         Neil Mitchell <ndmitchell@gmail.com>\ncopyright:          Neil Mitchell 2009-2021\nsynopsis:           Command line argument processing\ndescription:\n    This library provides an easy way to define command line parsers. Most users\n    will want to use the \"System.Console.CmdArgs.Implicit\" module, whose\n    documentation contains an example.\n    .\n    * \"System.Console.CmdArgs.Explicit\" provides a way to write command line\n      parsers for both single mode programs (most programs) and multiple\n      mode programs (e.g. darcs or cabal). Parsers are defined by constructing\n      a data structure.\n    .\n    * \"System.Console.CmdArgs.Implicit\" provides a way to concisely define\n      command line parsers, up to three times shorter than getopt. These parsers\n      are translated into the Explicit data type.\n    .\n    * \"System.Console.CmdArgs.GetOpt\" provides a wrapper allowing compatiblity\n      with existing getopt parsers, mapping to the Explicit data type.\n    .\n    For a general reference on what command line flags are commonly used,\n    see <http://www.faqs.org/docs/artu/ch10s05.html>.\nbug-reports:        https://github.com/ndmitchell/cmdargs/issues\nhomepage:           https://github.com/ndmitchell/cmdargs#readme\nextra-doc-files:\n    README.md\n    CHANGES.txt\ntested-with:        GHC==9.0, GHC==8.10, GHC==8.8, GHC==8.6, GHC==8.4, GHC==8.2, GHC==8.0\n\nsource-repository head\n    type:     git\n    location: https://github.com/ndmitchell/cmdargs.git\n\nflag testprog\n    default: False\n    manual: True\n    description: Build the test program\n\nflag quotation\n    default: True\n    manual: True\n    description: Build the Quote module\n\nlibrary\n    default-language: Haskell2010\n    build-depends:\n        base >= 4.4 && < 5,\n        filepath,\n        transformers >= 0.2,\n        process >= 1.0\n\n    if impl(ghc < 8.0)\n        build-depends: semigroups >= 0.18\n\n    if flag(quotation)\n        build-depends: template-haskell\n        exposed-modules: System.Console.CmdArgs.Quote\n        -- See bug #539 for why this magic is required\n        other-extensions: TemplateHaskell\n\n    exposed-modules:\n        System.Console.CmdArgs\n        System.Console.CmdArgs.Annotate\n        System.Console.CmdArgs.Default\n        System.Console.CmdArgs.Explicit\n        System.Console.CmdArgs.GetOpt\n        System.Console.CmdArgs.Implicit\n        System.Console.CmdArgs.Text\n        System.Console.CmdArgs.Helper\n        System.Console.CmdArgs.Verbosity\n\n    other-modules:\n        Data.Generics.Any\n        Data.Generics.Any.Prelude\n        System.Console.CmdArgs.Explicit.Complete\n        System.Console.CmdArgs.Explicit.ExpandArgsAt\n        System.Console.CmdArgs.Explicit.Help\n        System.Console.CmdArgs.Explicit.Process\n        System.Console.CmdArgs.Explicit.SplitJoin\n        System.Console.CmdArgs.Explicit.Type\n        System.Console.CmdArgs.Implicit.Ann\n        System.Console.CmdArgs.Implicit.Global\n        System.Console.CmdArgs.Implicit.Local\n        System.Console.CmdArgs.Implicit.Reader\n        System.Console.CmdArgs.Implicit.Reform\n        System.Console.CmdArgs.Implicit.Type\n        System.Console.CmdArgs.Implicit.UI\n\nexecutable cmdargs\n    default-language: Haskell2010\n    main-is: Main.hs\n    other-extensions: TemplateHaskell\n    build-depends:\n        base, transformers, filepath, process, template-haskell\n    if flag(testprog) && flag(quotation)\n        buildable: True\n    else\n        buildable: False\n\n    other-modules:\n        System.Console.CmdArgs.Test.All\n        System.Console.CmdArgs.Test.Explicit\n        System.Console.CmdArgs.Test.GetOpt\n        System.Console.CmdArgs.Test.Implicit\n        System.Console.CmdArgs.Test.Implicit.Diffy\n        System.Console.CmdArgs.Test.Implicit.HLint\n        System.Console.CmdArgs.Test.Implicit.Maker\n        System.Console.CmdArgs.Test.Implicit.Tests\n        System.Console.CmdArgs.Test.Implicit.Util\n        System.Console.CmdArgs.Test.SplitJoin\n        System.Console.CmdArgs.Test.Util\n";
    }