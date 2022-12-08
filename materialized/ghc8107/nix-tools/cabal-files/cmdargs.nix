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
    }