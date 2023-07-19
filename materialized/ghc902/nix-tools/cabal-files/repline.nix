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
      identifier = { name = "repline"; version = "0.4.2.0"; };
      license = "MIT";
      copyright = "2014-2022 Stephen Diehl";
      maintainer = "stephen.m.diehl@gmail.com";
      author = "Stephen Diehl";
      homepage = "https://github.com/sdiehl/repline";
      url = "";
      synopsis = "Haskeline wrapper for GHCi-like REPL interfaces.";
      description = "Haskeline wrapper for GHCi-like REPL interfaces. Composable with normal mtl transformers.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
          (hsPkgs."haskeline" or (errorHandler.buildDepError "haskeline"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."process" or (errorHandler.buildDepError "process"))
          ] ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8.0")) (hsPkgs."fail" or (errorHandler.buildDepError "fail"));
        buildable = true;
        };
      tests = {
        "prefix" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."repline" or (errorHandler.buildDepError "repline"))
            ];
          buildable = true;
          };
        "simple" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."process" or (errorHandler.buildDepError "process"))
            (hsPkgs."repline" or (errorHandler.buildDepError "repline"))
            ];
          buildable = true;
          };
        "stateful" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."repline" or (errorHandler.buildDepError "repline"))
            ];
          buildable = true;
          };
        "multiline" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."process" or (errorHandler.buildDepError "process"))
            (hsPkgs."repline" or (errorHandler.buildDepError "repline"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/repline-0.4.2.0.tar.gz";
      sha256 = "6181b3f106b4059eb1c86c4a442083c29335a1d9a3e7cfffecb962ec05b08d5a";
      });
    }) // {
    package-description-override = "name:               repline\nversion:            0.4.2.0\nsynopsis:           Haskeline wrapper for GHCi-like REPL interfaces.\nlicense:            MIT\nlicense-file:       LICENSE\nauthor:             Stephen Diehl\nmaintainer:         stephen.m.diehl@gmail.com\ncopyright:          2014-2022 Stephen Diehl\ncategory:           User Interfaces\nbuild-type:         Simple\nextra-source-files: README.md\ncabal-version:      >=1.10\ntested-with:\n  GHC ==8.2.2\n   || ==8.4.4\n   || ==8.6.2\n   || ==8.6.3\n   || ==8.6.4\n   || ==8.6.5\n   || ==8.8.1\n   || ==8.10.1\n   || ==8.10.7\n   || ==9.0.1\n   || ==9.2\n\nhomepage:           https://github.com/sdiehl/repline\nbug-reports:        https://github.com/sdiehl/repline/issues\ndescription:\n  Haskeline wrapper for GHCi-like REPL interfaces. Composable with normal mtl transformers.\n\nextra-source-files:\n  README.md\n  ChangeLog.md\n\nsource-repository head\n  type:     git\n  location: git@github.com:sdiehl/repline.git\n\nlibrary\n  hs-source-dirs:   src\n  exposed-modules:  System.Console.Repline\n  ghc-options:      -Wall\n  build-depends:\n      base        >=4.6  && <5.0\n    , containers  >=0.5  && <0.7\n    , exceptions  >=0.10 && <0.11\n    , haskeline   >=0.8  && <0.9\n    , mtl         >=2.2  && <2.4\n    , process     >=1.2  && <2.0\n\n  if !impl(ghc >=8.0)\n    build-depends: fail ==4.9.*\n\n  default-language: Haskell2010\n\ntest-suite prefix\n  type:             exitcode-stdio-1.0\n  main-is:          examples/Prefix.hs\n  default-language: Haskell2010\n  build-depends:\n      base\n    , containers\n    , mtl\n    , repline\n\ntest-suite simple\n  type:             exitcode-stdio-1.0\n  main-is:          examples/Simple.hs\n  default-language: Haskell2010\n  build-depends:\n      base\n    , containers\n    , mtl\n    , process\n    , repline\n\ntest-suite stateful\n  type:             exitcode-stdio-1.0\n  main-is:          examples/Stateful.hs\n  default-language: Haskell2010\n  build-depends:\n      base\n    , containers\n    , mtl\n    , repline\n\ntest-suite multiline\n  type:             exitcode-stdio-1.0\n  main-is:          examples/Multiline.hs\n  default-language: Haskell2010\n  build-depends:\n      base\n    , containers\n    , mtl\n    , process\n    , repline\n";
    }