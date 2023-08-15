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
    flags = {
      base-4-9 = true;
      template-haskell-2-11 = true;
      new-functor-classes = true;
      };
    package = {
      specVersion = "1.10";
      identifier = { name = "deriving-compat"; version = "0.6.3"; };
      license = "BSD-3-Clause";
      copyright = "(C) 2015-2017 Ryan Scott";
      maintainer = "Ryan Scott <ryan.gl.scott@gmail.com>";
      author = "Ryan Scott";
      homepage = "https://github.com/haskell-compat/deriving-compat";
      url = "";
      synopsis = "Backports of GHC deriving extensions";
      description = "@deriving-compat@ provides Template Haskell functions that\nmimic @deriving@ extensions that were introduced or modified\nin recent versions of GHC. Currently, the following\ntypeclasses/extensions are covered:\n\n* Deriving @Bounded@\n\n* Deriving @Enum@\n\n* Deriving @Ix@\n\n* Deriving @Eq@\n\n* Deriving @Ord@\n\n* Deriving @Read@\n\n* Deriving @Show@\n\n* @DeriveFoldable@\n\n* @DeriveFunctor@\n\n* @DeriveTraversable@\n\n* @GeneralizedNewtypeDeriving@ (with GHC 8.2 or later)\n\n* @DerivingVia@ (with GHC 8.2 or later)\n\nSee the \"Data.Deriving\" module for a full list of backported changes.\n\nIn addition, @deriving-compat@ also provides some additional\n@deriving@ functionality that has not yet been merged into\nupstream GHC. Aside from the GHC @deriving@ extensions\nmentioned above, @deriving-compat@ also permits deriving\ninstances of classes in the @Data.Functor.Classes@ module,\ncovering the @Eq1@, @Eq2@, @Ord1@, @Ord2@, @Read1@,\n@Read2@, @Show1@, and @Show2@ classes. This extra\nfunctionality is outside of the main scope of\n@deriving-compat@, as it does not backport extensions that\nexist in today's GHC. Nevertheless, the underlying Template\nHaskell machinery needed to derive @Eq@ and friends\nextends very naturally to @Eq1@ and friends, so this extra\nfunctionality is included in @deriving-compat@ as a\nconvenience.\n\nNote that some recent GHC typeclasses/extensions are not covered by this package:\n\n* @DeriveDataTypeable@\n\n* @DeriveGeneric@, which was introducted in GHC 7.2 for deriving\n@Generic@ instances, and modified in GHC 7.6 to allow derivation\nof @Generic1@ instances. Use @Generics.Deriving.TH@ from\n@<http://hackage.haskell.org/package/generic-deriving generic-deriving>@\nto derive @Generic(1)@ using Template Haskell.\n\n* @DeriveLift@, which was introduced in GHC 8.0 for deriving\n@Lift@ instances. Use @Language.Haskell.TH.Lift@ from\n@<http://hackage.haskell.org/package/th-lift th-lift>@\nto derive @Lift@ using Template Haskell.\n\n* The @Bifunctor@ typeclass, which was introduced in GHC 7.10,\nas well as the @Bifoldable@ and @Bitraversable@ typeclasses, which\nwere introduced in GHC 8.2. Use @Data.Bifunctor.TH@ from\n@<http://hackage.haskell.org/package/bifunctors bifunctors>@\nto derive these typeclasses using Template Haskell.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = (([
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
          (hsPkgs."th-abstraction" or (errorHandler.buildDepError "th-abstraction"))
          ] ++ [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          ]) ++ (if flags.template-haskell-2-11
          then [
            (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
            (hsPkgs."ghc-boot-th" or (errorHandler.buildDepError "ghc-boot-th"))
            ]
          else [
            (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
            ])) ++ (if flags.new-functor-classes
          then [
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."transformers-compat" or (errorHandler.buildDepError "transformers-compat"))
            ]
          else [
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            ]);
        buildable = true;
        };
      tests = {
        "spec" = {
          depends = ([
            (hsPkgs."base-compat" or (errorHandler.buildDepError "base-compat"))
            (hsPkgs."base-orphans" or (errorHandler.buildDepError "base-orphans"))
            (hsPkgs."deriving-compat" or (errorHandler.buildDepError "deriving-compat"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."tagged" or (errorHandler.buildDepError "tagged"))
            (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
            (hsPkgs."void" or (errorHandler.buildDepError "void"))
            ] ++ [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            ]) ++ (if flags.new-functor-classes
            then [
              (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
              (hsPkgs."transformers-compat" or (errorHandler.buildDepError "transformers-compat"))
              ]
            else [
              (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
              ]);
          build-tools = [
            (hsPkgs.buildPackages.hspec-discover.components.exes.hspec-discover or (pkgs.buildPackages.hspec-discover or (errorHandler.buildToolDepError "hspec-discover:hspec-discover")))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/deriving-compat-0.6.3.tar.gz";
      sha256 = "b1bf97012db9012a74af4b13cb3454afb7afa1db54c0f6e4bd7c10349e66692a";
      });
    }) // {
    package-description-override = "name:                deriving-compat\nversion:             0.6.3\nsynopsis:            Backports of GHC deriving extensions\ndescription:         @deriving-compat@ provides Template Haskell functions that\n                     mimic @deriving@ extensions that were introduced or modified\n                     in recent versions of GHC. Currently, the following\n                     typeclasses/extensions are covered:\n                     .\n                     * Deriving @Bounded@\n                     .\n                     * Deriving @Enum@\n                     .\n                     * Deriving @Ix@\n                     .\n                     * Deriving @Eq@\n                     .\n                     * Deriving @Ord@\n                     .\n                     * Deriving @Read@\n                     .\n                     * Deriving @Show@\n                     .\n                     * @DeriveFoldable@\n                     .\n                     * @DeriveFunctor@\n                     .\n                     * @DeriveTraversable@\n                     .\n                     * @GeneralizedNewtypeDeriving@ (with GHC 8.2 or later)\n                     .\n                     * @DerivingVia@ (with GHC 8.2 or later)\n                     .\n                     See the \"Data.Deriving\" module for a full list of backported changes.\n                     .\n                     In addition, @deriving-compat@ also provides some additional\n                     @deriving@ functionality that has not yet been merged into\n                     upstream GHC. Aside from the GHC @deriving@ extensions\n                     mentioned above, @deriving-compat@ also permits deriving\n                     instances of classes in the @Data.Functor.Classes@ module,\n                     covering the @Eq1@, @Eq2@, @Ord1@, @Ord2@, @Read1@,\n                     @Read2@, @Show1@, and @Show2@ classes. This extra\n                     functionality is outside of the main scope of\n                     @deriving-compat@, as it does not backport extensions that\n                     exist in today's GHC. Nevertheless, the underlying Template\n                     Haskell machinery needed to derive @Eq@ and friends\n                     extends very naturally to @Eq1@ and friends, so this extra\n                     functionality is included in @deriving-compat@ as a\n                     convenience.\n                     .\n                     Note that some recent GHC typeclasses/extensions are not covered by this package:\n                     .\n                     * @DeriveDataTypeable@\n                     .\n                     * @DeriveGeneric@, which was introducted in GHC 7.2 for deriving\n                       @Generic@ instances, and modified in GHC 7.6 to allow derivation\n                       of @Generic1@ instances. Use @Generics.Deriving.TH@ from\n                       @<http://hackage.haskell.org/package/generic-deriving generic-deriving>@\n                       to derive @Generic(1)@ using Template Haskell.\n                     .\n                     * @DeriveLift@, which was introduced in GHC 8.0 for deriving\n                       @Lift@ instances. Use @Language.Haskell.TH.Lift@ from\n                       @<http://hackage.haskell.org/package/th-lift th-lift>@\n                       to derive @Lift@ using Template Haskell.\n                     .\n                     * The @Bifunctor@ typeclass, which was introduced in GHC 7.10,\n                       as well as the @Bifoldable@ and @Bitraversable@ typeclasses, which\n                       were introduced in GHC 8.2. Use @Data.Bifunctor.TH@ from\n                       @<http://hackage.haskell.org/package/bifunctors bifunctors>@\n                       to derive these typeclasses using Template Haskell.\nhomepage:            https://github.com/haskell-compat/deriving-compat\nbug-reports:         https://github.com/haskell-compat/deriving-compat/issues\nlicense:             BSD3\nlicense-file:        LICENSE\nauthor:              Ryan Scott\nmaintainer:          Ryan Scott <ryan.gl.scott@gmail.com>\nstability:           Experimental\ncopyright:           (C) 2015-2017 Ryan Scott\ncategory:            Compatibility\nbuild-type:          Simple\nextra-source-files:  CHANGELOG.md, README.md\ntested-with:         GHC == 7.0.4\n                   , GHC == 7.2.2\n                   , GHC == 7.4.2\n                   , GHC == 7.6.3\n                   , GHC == 7.8.4\n                   , GHC == 7.10.3\n                   , GHC == 8.0.2\n                   , GHC == 8.2.2\n                   , GHC == 8.4.4\n                   , GHC == 8.6.5\n                   , GHC == 8.8.4\n                   , GHC == 8.10.7\n                   , GHC == 9.0.2\n                   , GHC == 9.2.6\n                   , GHC == 9.4.4\n                   , GHC == 9.6.1\ncabal-version:       >=1.10\n\nsource-repository head\n  type:                git\n  location:            https://github.com/haskell-compat/deriving-compat\n\nflag base-4-9\n  description:         Use base-4.9 or later.\n  default:             True\n\nflag template-haskell-2-11\n  description:         Use template-haskell-2.11.0.0 or later.\n  default:             True\n\nflag new-functor-classes\n  description:         Use a version of transformers or transformers-compat with a\n                       modern-style Data.Functor.Classes module. This flag cannot be\n                       used when building with transformers-0.4, since it comes with\n                       a different version of Data.Functor.Classes.\n  default:             True\n\nlibrary\n  exposed-modules:     Data.Deriving\n                       Data.Deriving.Internal\n\n                       Data.Bounded.Deriving\n                       Data.Bounded.Deriving.Internal\n                       Data.Deriving.Via\n                       Data.Deriving.Via.Internal\n                       Data.Enum.Deriving\n                       Data.Enum.Deriving.Internal\n                       Data.Eq.Deriving\n                       Data.Eq.Deriving.Internal\n                       Data.Foldable.Deriving\n                       Data.Functor.Deriving.Internal\n                       Data.Functor.Deriving\n                       Data.Ix.Deriving\n                       Data.Ix.Deriving.Internal\n                       Data.Ord.Deriving\n                       Data.Ord.Deriving.Internal\n                       Data.Traversable.Deriving\n                       Text.Read.Deriving\n                       Text.Read.Deriving.Internal\n                       Text.Show.Deriving\n                       Text.Show.Deriving.Internal\n  other-modules:       Paths_deriving_compat\n  build-depends:       containers          >= 0.1   && < 0.7\n                     , ghc-prim\n                     , th-abstraction      >= 0.4   && < 0.6\n\n  if flag(base-4-9)\n    build-depends:     base                >= 4.9   && < 5\n    cpp-options:       \"-DNEW_FUNCTOR_CLASSES\"\n  else\n    build-depends:     base                >= 4.3   && < 4.9\n\n  if flag(template-haskell-2-11)\n    build-depends:     template-haskell    >= 2.11  && < 2.21\n                     , ghc-boot-th\n  else\n    build-depends:     template-haskell    >= 2.5   && < 2.11\n\n  if flag(new-functor-classes)\n    build-depends:     transformers        (>= 0.2  && < 0.4) || (>= 0.5 && < 0.7)\n                     , transformers-compat >= 0.5\n    cpp-options:       \"-DNEW_FUNCTOR_CLASSES\"\n  else\n    build-depends:     transformers        == 0.4.*\n\n  hs-source-dirs:      src\n  default-language:    Haskell2010\n  ghc-options:         -Wall\n\ntest-suite spec\n  type:                exitcode-stdio-1.0\n  main-is:             Spec.hs\n  other-modules:       BoundedEnumIxSpec\n                       DerivingViaSpec\n                       EqSpec\n                       FunctorSpec\n                       OrdSpec\n                       ReadSpec\n                       ShowSpec\n                       GH6Spec\n                       GH24Spec\n                       GH27Spec\n                       GH31Spec\n\n                       Types.EqOrd\n                       Types.ReadShow\n  build-depends:       base-compat         >= 0.8.1  && < 1\n                     , base-orphans        >= 0.5    && < 1\n                     , deriving-compat\n                     , hspec               >= 1.8\n                     , QuickCheck          >= 2      && < 3\n                     , tagged              >= 0.7    && < 1\n                     , template-haskell    >= 2.5    && < 2.21\n                     , void                >= 0.5.10 && < 1\n  build-tool-depends:  hspec-discover:hspec-discover >= 1.8\n\n  if flag(base-4-9)\n    build-depends:     base                >= 4.9 && < 5\n    cpp-options:       \"-DNEW_FUNCTOR_CLASSES\"\n  else\n    build-depends:     base                >= 4.3 && < 4.9\n\n  if flag(new-functor-classes)\n    build-depends:     transformers        (>= 0.2 && < 0.4) || (>= 0.5 && < 0.7)\n                     , transformers-compat >= 0.5\n    cpp-options:       \"-DNEW_FUNCTOR_CLASSES\"\n  else\n    build-depends:     transformers        == 0.4.*\n\n  hs-source-dirs:      tests\n  default-language:    Haskell2010\n  ghc-options:         -Wall -threaded -rtsopts\n  if impl(ghc >= 8.6)\n    ghc-options:       -Wno-star-is-type\n  if impl(ghc >= 9.0)\n    ghc-options:       -fenable-th-splice-warnings\n";
    }