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
    flags = { transformers-0-4 = true; };
    package = {
      specVersion = "1.10";
      identifier = { name = "exceptions"; version = "0.10.7"; };
      license = "BSD-3-Clause";
      copyright = "Copyright (C) 2013-2015 Edward A. Kmett\nCopyright (C) 2012 Google Inc.";
      maintainer = "Edward A. Kmett <ekmett@gmail.com>";
      author = "Edward A. Kmett";
      homepage = "http://github.com/ekmett/exceptions/";
      url = "";
      synopsis = "Extensible optionally-pure exceptions";
      description = "Extensible optionally-pure exceptions.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = ([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          ] ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8.0")) (hsPkgs."fail" or (errorHandler.buildDepError "fail"))) ++ (if flags.transformers-0-4
          then [
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            ]
          else [
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."transformers-compat" or (errorHandler.buildDepError "transformers-compat"))
            ]);
        buildable = true;
        };
      tests = {
        "exceptions-tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
            (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
            (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."test-framework" or (errorHandler.buildDepError "test-framework"))
            (hsPkgs."test-framework-hunit" or (errorHandler.buildDepError "test-framework-hunit"))
            (hsPkgs."test-framework-quickcheck2" or (errorHandler.buildDepError "test-framework-quickcheck2"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            ] ++ (if flags.transformers-0-4
            then [
              (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
              ]
            else [
              (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
              (hsPkgs."transformers-compat" or (errorHandler.buildDepError "transformers-compat"))
              ]);
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/exceptions-0.10.7.tar.gz";
      sha256 = "9a42ade4c8b53d8da5350e8e0e2929f4ef128c4b8591b120656455310b546049";
      });
    }) // {
    package-description-override = "name:          exceptions\r\ncategory:      Control, Exceptions, Monad\r\nversion:       0.10.7\r\nx-revision: 1\r\ncabal-version: >= 1.10\r\nlicense:       BSD3\r\nlicense-file:  LICENSE\r\nauthor:        Edward A. Kmett\r\nmaintainer:    Edward A. Kmett <ekmett@gmail.com>\r\nstability:     provisional\r\nhomepage:      http://github.com/ekmett/exceptions/\r\nbug-reports:   http://github.com/ekmett/exceptions/issues\r\ncopyright:     Copyright (C) 2013-2015 Edward A. Kmett\r\n               Copyright (C) 2012 Google Inc.\r\nbuild-type:    Simple\r\ntested-with:   GHC == 7.4.2\r\n             , GHC == 7.6.3\r\n             , GHC == 7.8.4\r\n             , GHC == 7.10.3\r\n             , GHC == 8.0.2\r\n             , GHC == 8.2.2\r\n             , GHC == 8.4.4\r\n             , GHC == 8.6.5\r\n             , GHC == 8.8.4\r\n             , GHC == 8.10.7\r\n             , GHC == 9.0.2\r\n             , GHC == 9.2.2\r\nsynopsis:      Extensible optionally-pure exceptions\r\ndescription:   Extensible optionally-pure exceptions.\r\n\r\nextra-source-files:\r\n  .ghci\r\n  .gitignore\r\n  .vim.custom\r\n  AUTHORS.markdown\r\n  README.markdown\r\n  CHANGELOG.markdown\r\n\r\nsource-repository head\r\n  type: git\r\n  location: git://github.com/ekmett/exceptions.git\r\n\r\nflag transformers-0-4\r\n  description: Use @transformers-0.4@ or later.\r\n  default:     True\r\n\r\nlibrary\r\n  build-depends:\r\n    base                       >= 4.5      && < 5,\r\n    stm                        >= 2.2      && < 3,\r\n    template-haskell           >= 2.7      && < 2.21,\r\n    mtl                        >= 2.0      && < 2.4\r\n\r\n  if !impl(ghc >= 8.0)\r\n    build-depends: fail        == 4.9.*\r\n\r\n  if flag(transformers-0-4)\r\n    build-depends:\r\n      transformers             >= 0.4      && < 0.7\r\n  else\r\n    build-depends:\r\n      transformers             >= 0.2      && < 0.4,\r\n      transformers-compat      >= 0.3      && < 0.8\r\n\r\n  exposed-modules:\r\n    Control.Monad.Catch\r\n    Control.Monad.Catch.Pure\r\n\r\n  ghc-options: -Wall -fwarn-tabs -O2\r\n  hs-source-dirs: src\r\n  default-language: Haskell2010\r\n\r\ntest-suite exceptions-tests\r\n  main-is: Tests.hs\r\n  other-modules: Control.Monad.Catch.Tests\r\n  hs-source-dirs: tests\r\n  ghc-options: -Wall -fwarn-tabs\r\n  default-language: Haskell2010\r\n  type: exitcode-stdio-1.0\r\n  build-depends:\r\n    base,\r\n    exceptions,\r\n    stm,\r\n    template-haskell,\r\n    mtl                        >= 2.0,\r\n    test-framework             >= 0.8      && < 0.9,\r\n    test-framework-hunit       >= 0.3      && < 0.4,\r\n    test-framework-quickcheck2 >= 0.3      && < 0.4,\r\n    QuickCheck                 >= 2.5      && < 2.15\r\n\r\n  if flag(transformers-0-4)\r\n    build-depends:\r\n      transformers             >= 0.4      && < 0.7\r\n  else\r\n    build-depends:\r\n      transformers             >= 0.2      && < 0.4,\r\n      transformers-compat      >= 0.3      && < 0.8\r\n";
    }