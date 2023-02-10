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
    flags = { semigroups = true; tagged = true; };
    package = {
      specVersion = "1.10";
      identifier = { name = "bifunctors"; version = "5.5.13"; };
      license = "BSD-3-Clause";
      copyright = "Copyright (C) 2008-2016 Edward A. Kmett";
      maintainer = "Edward A. Kmett <ekmett@gmail.com>";
      author = "Edward A. Kmett";
      homepage = "http://github.com/ekmett/bifunctors/";
      url = "";
      synopsis = "Bifunctors";
      description = "Bifunctors.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = (((([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."base-orphans" or (errorHandler.buildDepError "base-orphans"))
          (hsPkgs."comonad" or (errorHandler.buildDepError "comonad"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."th-abstraction" or (errorHandler.buildDepError "th-abstraction"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          ] ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).gt "8.2")) (hsPkgs."transformers-compat" or (errorHandler.buildDepError "transformers-compat"))) ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8.0")) (hsPkgs."fail" or (errorHandler.buildDepError "fail"))) ++ (pkgs.lib).optional (flags.tagged) (hsPkgs."tagged" or (errorHandler.buildDepError "tagged"))) ++ (pkgs.lib).optional (flags.semigroups && !(compiler.isGhc && (compiler.version).ge "8.0")) (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))) ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).ge "7.2" && (compiler.isGhc && (compiler.version).lt "7.5")) (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"));
        buildable = true;
        };
      tests = {
        "bifunctors-spec" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bifunctors" or (errorHandler.buildDepError "bifunctors"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."transformers-compat" or (errorHandler.buildDepError "transformers-compat"))
            ];
          build-tools = [
            (hsPkgs.buildPackages.hspec-discover.components.exes.hspec-discover or (pkgs.buildPackages.hspec-discover or (errorHandler.buildToolDepError "hspec-discover:hspec-discover")))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/bifunctors-5.5.13.tar.gz";
      sha256 = "26cc27ade3796cf03bd680f98ebb7b8c0d7fd1b67c1894dc3635f734fba7dbd7";
      });
    }) // {
    package-description-override = "name:          bifunctors\ncategory:      Data, Functors\nversion:       5.5.13\nlicense:       BSD3\ncabal-version: >= 1.10\nlicense-file:  LICENSE\nauthor:        Edward A. Kmett\nmaintainer:    Edward A. Kmett <ekmett@gmail.com>\nstability:     provisional\nhomepage:      http://github.com/ekmett/bifunctors/\nbug-reports:   http://github.com/ekmett/bifunctors/issues\ncopyright:     Copyright (C) 2008-2016 Edward A. Kmett\nsynopsis:      Bifunctors\ndescription:   Bifunctors.\nbuild-type:    Simple\ntested-with:   GHC == 7.0.4\n             , GHC == 7.2.2\n             , GHC == 7.4.2\n             , GHC == 7.6.3\n             , GHC == 7.8.4\n             , GHC == 7.10.3\n             , GHC == 8.0.2\n             , GHC == 8.2.2\n             , GHC == 8.4.4\n             , GHC == 8.6.5\n             , GHC == 8.8.4\n             , GHC == 8.10.7\n             , GHC == 9.0.2\n             , GHC == 9.2.2\nextra-source-files:\n  CHANGELOG.markdown\n  README.markdown\n  include/bifunctors-common.h\n\nsource-repository head\n  type: git\n  location: https://github.com/ekmett/bifunctors.git\n\nflag semigroups\n  default: True\n  manual: True\n  description:\n    You can disable the use of the `semigroups` package using `-f-semigroups`.\n    .\n    Disabing this is an unsupported configuration, but it may be useful for accelerating builds in sandboxes for expert users.\n\nflag tagged\n  default: True\n  manual: True\n  description:\n    You can disable the use of the `tagged` package using `-f-tagged`.\n    .\n    Disabing this is an unsupported configuration, but it may be useful for accelerating builds in sandboxes for expert users.\n\nlibrary\n  hs-source-dirs: src\n  include-dirs: include\n  includes: bifunctors-common.h\n  build-depends:\n    base                >= 4.3   && < 5,\n    base-orphans        >= 0.8.4 && < 1,\n    comonad             >= 5.0.7 && < 6,\n    containers          >= 0.2   && < 0.7,\n    template-haskell    >= 2.4   && < 2.20,\n    th-abstraction      >= 0.4.2.0 && < 0.5,\n    transformers        >= 0.3   && < 0.7\n\n  if !impl(ghc > 8.2)\n    build-depends: transformers-compat >= 0.5 && < 0.8\n\n  if !impl(ghc >= 8.0)\n    build-depends: fail == 4.9.*\n\n  if flag(tagged)\n    build-depends: tagged >= 0.8.6 && < 1\n\n  if flag(semigroups) && !impl(ghc >= 8.0)\n    build-depends: semigroups >= 0.18.5 && < 1\n\n  if impl(ghc<7.9)\n    hs-source-dirs: old-src/ghc709\n    exposed-modules: Data.Bifunctor\n\n  if impl(ghc<8.1)\n    hs-source-dirs: old-src/ghc801\n    exposed-modules:\n      Data.Bifoldable\n      Data.Bitraversable\n\n  if impl(ghc>=7.2) && impl(ghc<7.5)\n    build-depends: ghc-prim == 0.2.0.0\n\n  exposed-modules:\n    Data.Biapplicative\n    Data.Bifunctor.Biap\n    Data.Bifunctor.Biff\n    Data.Bifunctor.Clown\n    Data.Bifunctor.Fix\n    Data.Bifunctor.Flip\n    Data.Bifunctor.Functor\n    Data.Bifunctor.Join\n    Data.Bifunctor.Joker\n    Data.Bifunctor.Product\n    Data.Bifunctor.Sum\n    Data.Bifunctor.Tannen\n    Data.Bifunctor.TH\n    Data.Bifunctor.Wrapped\n\n  other-modules:\n    Data.Bifunctor.TH.Internal\n    Paths_bifunctors\n\n  ghc-options: -Wall\n  default-language: Haskell2010\n\n  if impl(ghc >= 9.0)\n    -- these flags may abort compilation with GHC-8.10\n    -- https://gitlab.haskell.org/ghc/ghc/-/merge_requests/3295\n    ghc-options: -Winferred-safe-imports -Wmissing-safe-haskell-mode\n\ntest-suite bifunctors-spec\n  type: exitcode-stdio-1.0\n  hs-source-dirs: tests\n  main-is: Spec.hs\n  other-modules: BifunctorSpec T89Spec\n  ghc-options: -Wall\n  if impl(ghc >= 8.6)\n    ghc-options: -Wno-star-is-type\n  default-language: Haskell2010\n  build-tool-depends: hspec-discover:hspec-discover >= 1.8\n  build-depends:\n    base                >= 4   && < 5,\n    bifunctors,\n    hspec               >= 1.8,\n    QuickCheck          >= 2   && < 3,\n    template-haskell,\n    transformers,\n    transformers-compat\n\n";
    }