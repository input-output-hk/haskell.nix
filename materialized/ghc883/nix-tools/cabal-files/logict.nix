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
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = { name = "logict"; version = "0.7.0.3"; };
      license = "BSD-3-Clause";
      copyright = "Copyright (c) 2007-2014, Dan Doel,\nCopyright (c) 2011-2013, Edward Kmett,\nCopyright (c) 2014, Roman Cheplyaka";
      maintainer = "Andrew Lelechenko <andrew.lelechenko@gmail.com>";
      author = "Dan Doel";
      homepage = "https://github.com/Bodigrim/logict#readme";
      url = "";
      synopsis = "A backtracking logic-programming monad.";
      description = "A continuation-based, backtracking, logic programming monad.\nAn adaptation of the two-continuation implementation found\nin the paper \"Backtracking, Interleaving, and Terminating\nMonad Transformers\" available here:\n<http://okmij.org/ftp/papers/LogicT.pdf>";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          ] ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).lt "8.0") (hsPkgs."fail" or (errorHandler.buildDepError "fail"));
        buildable = true;
        };
      tests = {
        "logict-tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."logict" or (errorHandler.buildDepError "logict"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/logict-0.7.0.3.tar.gz";
      sha256 = "791ce86e2d40d08f487523541425c70b6bf9f6db2dc1e0e9b0b99eab7284515f";
      });
    }) // {
    package-description-override = "name: logict\r\nversion: 0.7.0.3\r\nx-revision: 1\r\nlicense: BSD3\r\nlicense-file: LICENSE\r\ncopyright:\r\n  Copyright (c) 2007-2014, Dan Doel,\r\n  Copyright (c) 2011-2013, Edward Kmett,\r\n  Copyright (c) 2014, Roman Cheplyaka\r\nmaintainer: Andrew Lelechenko <andrew.lelechenko@gmail.com>\r\nauthor: Dan Doel\r\nhomepage: https://github.com/Bodigrim/logict#readme\r\nsynopsis: A backtracking logic-programming monad.\r\ndescription:\r\n  A continuation-based, backtracking, logic programming monad.\r\n  An adaptation of the two-continuation implementation found\r\n  in the paper \"Backtracking, Interleaving, and Terminating\r\n  Monad Transformers\" available here:\r\n  <http://okmij.org/ftp/papers/LogicT.pdf>\r\ncategory: Control\r\nbuild-type: Simple\r\nextra-source-files:\r\n  changelog.md\r\ncabal-version: >=1.10\r\ntested-with: GHC ==7.4.2 GHC ==7.6.3 GHC ==7.8.4 GHC ==7.10.3 GHC ==8.0.2 GHC ==8.2.2 GHC ==8.4.4 GHC ==8.6.5 GHC ==8.8.3 GHC ==8.10.1\r\n\r\nsource-repository head\r\n  type: git\r\n  location: https://github.com/Bodigrim/logict\r\n\r\nlibrary\r\n  exposed-modules:\r\n    Control.Monad.Logic\r\n    Control.Monad.Logic.Class\r\n  default-language: Haskell2010\r\n  ghc-options: -O2 -Wall\r\n  build-depends:\r\n    base >=4.5 && <5,\r\n    mtl >=2 && <2.3\r\n\r\n  if impl(ghc <8.0)\r\n    build-depends:\r\n      fail -any\r\n\r\ntest-suite logict-tests\r\n  type: exitcode-stdio-1.0\r\n  main-is: Test.hs\r\n  default-language: Haskell2010\r\n  ghc-options: -Wall\r\n  build-depends:\r\n    base >=2 && <5,\r\n    logict -any,\r\n    mtl >=2 && <2.3,\r\n    tasty,\r\n    tasty-hunit\r\n  hs-source-dirs: test\r\n";
    }