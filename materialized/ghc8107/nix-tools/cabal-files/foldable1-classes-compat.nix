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
    flags = { tagged = true; };
    package = {
      specVersion = "1.10";
      identifier = { name = "foldable1-classes-compat"; version = "0.1"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Ryan Scott <ryan.gl.scott@gmail.com>";
      author = "Edward Kmett, Oleg Grenrus";
      homepage = "https://github.com/haskell-compat/foldable1-classes-compat";
      url = "";
      synopsis = "Compatibility package for the Foldable1 and Bifoldable1 type classes";
      description = "A compatibility package for the @Foldable1@ and @Bifoldable1@ type classes,\nwhich were introduced in @base-4.18.0.0@ (GHC 9.6.1). For more information,\nsee <https://github.com/haskell/core-libraries-committee/issues/9 this Core\nLibraries Committee proposal>.\n\n@Foldable1@ and @Bifoldable1@ classify non-empty data structures that can be\nfolded to a summary value.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = (((((([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          ] ++ (pkgs.lib).optionals (!(compiler.isGhc && (compiler.version).ge "9.6")) [
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          ]) ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8.6")) (hsPkgs."base-orphans" or (errorHandler.buildDepError "base-orphans"))) ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8.2")) (hsPkgs."bifunctor-classes-compat" or (errorHandler.buildDepError "bifunctor-classes-compat"))) ++ (pkgs.lib).optionals (!(compiler.isGhc && (compiler.version).ge "8.0")) [
          (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
          (hsPkgs."transformers-compat" or (errorHandler.buildDepError "transformers-compat"))
          ]) ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "7.2")) (hsPkgs."generic-deriving" or (errorHandler.buildDepError "generic-deriving"))) ++ (pkgs.lib).optional (flags.tagged) (hsPkgs."tagged" or (errorHandler.buildDepError "tagged"))) ++ (if compiler.isGhc && (compiler.version).ge "9.0"
          then [
            (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
            ]
          else (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "7.6")) (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim")));
        buildable = true;
        };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."foldable1-classes-compat" or (errorHandler.buildDepError "foldable1-classes-compat"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."quickcheck-instances" or (errorHandler.buildDepError "quickcheck-instances"))
            (hsPkgs."test-framework" or (errorHandler.buildDepError "test-framework"))
            (hsPkgs."test-framework-quickcheck2" or (errorHandler.buildDepError "test-framework-quickcheck2"))
            ] ++ (pkgs.lib).optionals (!(compiler.isGhc && (compiler.version).ge "8.0")) [
            (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
            (hsPkgs."transformers-compat" or (errorHandler.buildDepError "transformers-compat"))
            ];
          buildable = if !(compiler.isGhc && (compiler.version).ge "7.4")
            then false
            else true;
          };
        };
      benchmarks = {
        "bench" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."foldable1-classes-compat" or (errorHandler.buildDepError "foldable1-classes-compat"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            ] ++ (pkgs.lib).optionals (!(compiler.isGhc && (compiler.version).ge "8.0")) [
            (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
            (hsPkgs."transformers-compat" or (errorHandler.buildDepError "transformers-compat"))
            ];
          buildable = if !(compiler.isGhc && (compiler.version).ge "7.6")
            then false
            else true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/foldable1-classes-compat-0.1.tar.gz";
      sha256 = "d057c3f358e1a6b72c73519bc64ba6aa959f066c08fed69f73258555ef95ff12";
      });
    }) // {
    package-description-override = "cabal-version: >=1.10\r\nname:          foldable1-classes-compat\r\nversion:       0.1\r\nx-revision: 2\r\nsynopsis:\r\n  Compatibility package for the Foldable1 and Bifoldable1 type classes\r\n\r\ndescription:\r\n  A compatibility package for the @Foldable1@ and @Bifoldable1@ type classes,\r\n  which were introduced in @base-4.18.0.0@ (GHC 9.6.1). For more information,\r\n  see <https://github.com/haskell/core-libraries-committee/issues/9 this Core\r\n  Libraries Committee proposal>.\r\n  .\r\n  @Foldable1@ and @Bifoldable1@ classify non-empty data structures that can be\r\n  folded to a summary value.\r\n\r\nlicense:       BSD3\r\nmaintainer:    Ryan Scott <ryan.gl.scott@gmail.com>\r\nauthor:        Edward Kmett, Oleg Grenrus\r\nhomepage:      https://github.com/haskell-compat/foldable1-classes-compat\r\nbug-reports:   https://github.com/haskell-compat/foldable1-classes-compat/issues\r\ncategory:      Data, Compatibility\r\nlicense-file:  LICENSE\r\nbuild-type:    Simple\r\nextra-source-files:\r\n  CHANGELOG.markdown\r\n  README.markdown\r\ntested-with:\r\n  GHC ==7.0.4\r\n   || ==7.2.2\r\n   || ==7.4.2\r\n   || ==7.6.3\r\n   || ==7.8.4\r\n   || ==7.10.3\r\n   || ==8.0.2\r\n   || ==8.2.2\r\n   || ==8.4.4\r\n   || ==8.6.5\r\n   || ==8.8.4\r\n   || ==8.10.7\r\n   || ==9.0.2\r\n   || ==9.2.5\r\n   || ==9.4.4\r\n   || ==9.6.1\r\n\r\n-- , GHCJS ==8.4\r\n\r\nsource-repository head\r\n  type:     git\r\n  location: https://github.com/haskell-compat/foldable1-classes-compat.git\r\n\r\nflag tagged\r\n  description:\r\n    You can disable the use of the `tagged` package using `-f-tagged`.\r\n    .\r\n    Disabling this is an unsupported configuration, but it may be useful for accelerating builds in sandboxes for expert users.\r\n\r\n  default:     True\r\n  manual:      True\r\n\r\nlibrary\r\n  default-language: Haskell2010\r\n  ghc-options:      -Wall\r\n  build-depends:    base >=4.3 && <4.19\r\n\r\n  if !impl(ghc >= 9.6)\r\n    hs-source-dirs: src\r\n    build-depends:\r\n        containers    >=0.4 && <0.7\r\n      , transformers  >=0.3 && <0.7\r\n    exposed-modules:\r\n      Data.Foldable1\r\n      Data.Bifoldable1\r\n\r\n  if !impl(ghc >=8.6)\r\n    build-depends: base-orphans >=0.8.1 && <0.10\r\n\r\n  if !impl(ghc >=8.2)\r\n    build-depends: bifunctor-classes-compat >=0.1 && <0.2\r\n\r\n  if !impl(ghc >=8.0)\r\n    build-depends:\r\n        semigroups           >=0.18.5 && <0.21\r\n      , transformers-compat  >=0.6    && <0.8\r\n\r\n  if !impl(ghc >= 7.2)\r\n    build-depends: generic-deriving >=1.14 && <1.15\r\n\r\n  if flag(tagged)\r\n    build-depends: tagged >=0.4.4 && <1\r\n\r\n  if impl(ghc >= 9.0)\r\n    build-depends: ghc-prim >= 0.7 && <0.11\r\n  else\r\n    if !impl(ghc >=7.6)\r\n      build-depends: ghc-prim\r\n\r\ntest-suite test\r\n  default-language: Haskell2010\r\n  type:             exitcode-stdio-1.0\r\n  ghc-options:      -Wall\r\n  hs-source-dirs:   test\r\n  main-is:          Tests.hs\r\n\r\n  -- because of quickcheck-instances\r\n  if !impl(ghc >=7.4)\r\n    buildable: False\r\n\r\n  build-depends:\r\n      base\r\n    , containers\r\n    , foldable1-classes-compat\r\n    , transformers\r\n\r\n  if !impl(ghc >=8.0)\r\n    build-depends:\r\n        semigroups\r\n      , transformers-compat\r\n\r\n  build-depends:\r\n      QuickCheck                  >=2.13.2  && <2.15\r\n    , quickcheck-instances        >=0.3.27  && <0.4\r\n    , test-framework              >=0.8.2.0 && <0.9\r\n    , test-framework-quickcheck2  >=0.3.0.5 && <0.4\r\n\r\nbenchmark bench\r\n  default-language: Haskell2010\r\n  type:             exitcode-stdio-1.0\r\n  ghc-options:      -Wall\r\n  hs-source-dirs:   bench\r\n  main-is:          Bench.hs\r\n\r\n  if !impl(ghc >=7.6)\r\n    buildable: False\r\n\r\n  build-depends:\r\n      base\r\n    , containers\r\n    , foldable1-classes-compat\r\n    , transformers\r\n\r\n  if !impl(ghc >=8.0)\r\n    build-depends:\r\n        semigroups\r\n      , transformers-compat\r\n\r\n  build-depends:\r\n      criterion  >=1.5.6.1 && <1.7\r\n    , deepseq    >=1.3     && <1.5\r\n";
    }