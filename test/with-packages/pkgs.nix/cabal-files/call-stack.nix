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
      specVersion = "1.12";
      identifier = { name = "call-stack"; version = "0.4.0"; };
      license = "MIT";
      copyright = "";
      maintainer = "Simon Hengel <sol@typeful.net>";
      author = "";
      homepage = "https://github.com/sol/call-stack#readme";
      url = "";
      synopsis = "Use GHC call-stacks in a backward compatible way";
      description = "";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          ] ++ (pkgs.lib).optional (system.isWindows) (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"));
        buildable = true;
        };
      tests = {
        "spec" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."call-stack" or (errorHandler.buildDepError "call-stack"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."nanospec" or (errorHandler.buildDepError "nanospec"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/call-stack-0.4.0.tar.gz";
      sha256 = "430bcf8a3404f7e55319573c0b807b1356946f0c8f289bb3d9afb279c636b87b";
      });
    }) // {
    package-description-override = "cabal-version: 1.12\n\n-- This file has been generated from package.yaml by hpack version 0.34.4.\n--\n-- see: https://github.com/sol/hpack\n\nname:           call-stack\nversion:        0.4.0\nsynopsis:       Use GHC call-stacks in a backward compatible way\ncategory:       Data\nhomepage:       https://github.com/sol/call-stack#readme\nbug-reports:    https://github.com/sol/call-stack/issues\nmaintainer:     Simon Hengel <sol@typeful.net>\nlicense:        MIT\nlicense-file:   LICENSE\nbuild-type:     Simple\n\nsource-repository head\n  type: git\n  location: https://github.com/sol/call-stack\n\nlibrary\n  hs-source-dirs:\n      src\n  ghc-options: -Wall\n  build-depends:\n      base ==4.*\n  if os(windows)\n    cpp-options: -DWINDOWS\n    build-depends:\n        filepath\n  exposed-modules:\n      Data.CallStack\n  other-modules:\n      Data.SrcLoc\n      Paths_call_stack\n  default-language: Haskell2010\n\ntest-suite spec\n  type: exitcode-stdio-1.0\n  main-is: Spec.hs\n  hs-source-dirs:\n      test\n  ghc-options: -Wall\n  build-depends:\n      base ==4.*\n    , call-stack\n    , filepath\n    , nanospec\n  other-modules:\n      Data.CallStackSpec\n      Example\n      Util\n      Paths_call_stack\n  default-language: Haskell2010\n";
    }