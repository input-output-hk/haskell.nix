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
      identifier = { name = "deriving-compat"; version = "0.6.1"; };
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
      url = "http://hackage.haskell.org/package/deriving-compat-0.6.1.tar.gz";
      sha256 = "6bfda1bb730fdad50251f7875dff2802fea3388cfd4ea232f34a03218cae9d26";
      });
    }) // {
    package-description-override = "name:                deriving-compat\r\nversion:             0.6.1\r\nx-revision: 1\r\nsynopsis:            Backports of GHC deriving extensions\r\ndescription:         @deriving-compat@ provides Template Haskell functions that\r\n                     mimic @deriving@ extensions that were introduced or modified\r\n                     in recent versions of GHC. Currently, the following\r\n                     typeclasses/extensions are covered:\r\n                     .\r\n                     * Deriving @Bounded@\r\n                     .\r\n                     * Deriving @Enum@\r\n                     .\r\n                     * Deriving @Ix@\r\n                     .\r\n                     * Deriving @Eq@\r\n                     .\r\n                     * Deriving @Ord@\r\n                     .\r\n                     * Deriving @Read@\r\n                     .\r\n                     * Deriving @Show@\r\n                     .\r\n                     * @DeriveFoldable@\r\n                     .\r\n                     * @DeriveFunctor@\r\n                     .\r\n                     * @DeriveTraversable@\r\n                     .\r\n                     * @GeneralizedNewtypeDeriving@ (with GHC 8.2 or later)\r\n                     .\r\n                     * @DerivingVia@ (with GHC 8.2 or later)\r\n                     .\r\n                     See the \"Data.Deriving\" module for a full list of backported changes.\r\n                     .\r\n                     In addition, @deriving-compat@ also provides some additional\r\n                     @deriving@ functionality that has not yet been merged into\r\n                     upstream GHC. Aside from the GHC @deriving@ extensions\r\n                     mentioned above, @deriving-compat@ also permits deriving\r\n                     instances of classes in the @Data.Functor.Classes@ module,\r\n                     covering the @Eq1@, @Eq2@, @Ord1@, @Ord2@, @Read1@,\r\n                     @Read2@, @Show1@, and @Show2@ classes. This extra\r\n                     functionality is outside of the main scope of\r\n                     @deriving-compat@, as it does not backport extensions that\r\n                     exist in today's GHC. Nevertheless, the underlying Template\r\n                     Haskell machinery needed to derive @Eq@ and friends\r\n                     extends very naturally to @Eq1@ and friends, so this extra\r\n                     functionality is included in @deriving-compat@ as a\r\n                     convenience.\r\n                     .\r\n                     Note that some recent GHC typeclasses/extensions are not covered by this package:\r\n                     .\r\n                     * @DeriveDataTypeable@\r\n                     .\r\n                     * @DeriveGeneric@, which was introducted in GHC 7.2 for deriving\r\n                       @Generic@ instances, and modified in GHC 7.6 to allow derivation\r\n                       of @Generic1@ instances. Use @Generics.Deriving.TH@ from\r\n                       @<http://hackage.haskell.org/package/generic-deriving generic-deriving>@\r\n                       to derive @Generic(1)@ using Template Haskell.\r\n                     .\r\n                     * @DeriveLift@, which was introduced in GHC 8.0 for deriving\r\n                       @Lift@ instances. Use @Language.Haskell.TH.Lift@ from\r\n                       @<http://hackage.haskell.org/package/th-lift th-lift>@\r\n                       to derive @Lift@ using Template Haskell.\r\n                     .\r\n                     * The @Bifunctor@ typeclass, which was introduced in GHC 7.10,\r\n                       as well as the @Bifoldable@ and @Bitraversable@ typeclasses, which\r\n                       were introduced in GHC 8.2. Use @Data.Bifunctor.TH@ from\r\n                       @<http://hackage.haskell.org/package/bifunctors bifunctors>@\r\n                       to derive these typeclasses using Template Haskell.\r\nhomepage:            https://github.com/haskell-compat/deriving-compat\r\nbug-reports:         https://github.com/haskell-compat/deriving-compat/issues\r\nlicense:             BSD3\r\nlicense-file:        LICENSE\r\nauthor:              Ryan Scott\r\nmaintainer:          Ryan Scott <ryan.gl.scott@gmail.com>\r\nstability:           Experimental\r\ncopyright:           (C) 2015-2017 Ryan Scott\r\ncategory:            Compatibility\r\nbuild-type:          Simple\r\nextra-source-files:  CHANGELOG.md, README.md\r\ntested-with:         GHC == 7.0.4\r\n                   , GHC == 7.2.2\r\n                   , GHC == 7.4.2\r\n                   , GHC == 7.6.3\r\n                   , GHC == 7.8.4\r\n                   , GHC == 7.10.3\r\n                   , GHC == 8.0.2\r\n                   , GHC == 8.2.2\r\n                   , GHC == 8.4.4\r\n                   , GHC == 8.6.5\r\n                   , GHC == 8.8.4\r\n                   , GHC == 8.10.7\r\n                   , GHC == 9.0.2\r\n                   , GHC == 9.2.2\r\ncabal-version:       >=1.10\r\n\r\nsource-repository head\r\n  type:                git\r\n  location:            https://github.com/haskell-compat/deriving-compat\r\n\r\nflag base-4-9\r\n  description:         Use base-4.9 or later.\r\n  default:             True\r\n\r\nflag template-haskell-2-11\r\n  description:         Use template-haskell-2.11.0.0 or later.\r\n  default:             True\r\n\r\nflag new-functor-classes\r\n  description:         Use a version of transformers or transformers-compat with a\r\n                       modern-style Data.Functor.Classes module. This flag cannot be\r\n                       used when building with transformers-0.4, since it comes with\r\n                       a different version of Data.Functor.Classes.\r\n  default:             True\r\n\r\nlibrary\r\n  exposed-modules:     Data.Deriving\r\n                       Data.Deriving.Internal\r\n\r\n                       Data.Bounded.Deriving\r\n                       Data.Bounded.Deriving.Internal\r\n                       Data.Deriving.Via\r\n                       Data.Deriving.Via.Internal\r\n                       Data.Enum.Deriving\r\n                       Data.Enum.Deriving.Internal\r\n                       Data.Eq.Deriving\r\n                       Data.Eq.Deriving.Internal\r\n                       Data.Foldable.Deriving\r\n                       Data.Functor.Deriving.Internal\r\n                       Data.Functor.Deriving\r\n                       Data.Ix.Deriving\r\n                       Data.Ix.Deriving.Internal\r\n                       Data.Ord.Deriving\r\n                       Data.Ord.Deriving.Internal\r\n                       Data.Traversable.Deriving\r\n                       Text.Read.Deriving\r\n                       Text.Read.Deriving.Internal\r\n                       Text.Show.Deriving\r\n                       Text.Show.Deriving.Internal\r\n  other-modules:       Paths_deriving_compat\r\n  build-depends:       containers          >= 0.1   && < 0.7\r\n                     , ghc-prim\r\n                     , th-abstraction      >= 0.4   && < 0.5\r\n\r\n  if flag(base-4-9)\r\n    build-depends:     base                >= 4.9   && < 5\r\n    cpp-options:       \"-DNEW_FUNCTOR_CLASSES\"\r\n  else\r\n    build-depends:     base                >= 4.3   && < 4.9\r\n\r\n  if flag(template-haskell-2-11)\r\n    build-depends:     template-haskell    >= 2.11  && < 2.20\r\n                     , ghc-boot-th\r\n  else\r\n    build-depends:     template-haskell    >= 2.5   && < 2.11\r\n\r\n  if flag(new-functor-classes)\r\n    build-depends:     transformers        (>= 0.2  && < 0.4) || (>= 0.5 && < 0.7)\r\n                     , transformers-compat >= 0.5\r\n    cpp-options:       \"-DNEW_FUNCTOR_CLASSES\"\r\n  else\r\n    build-depends:     transformers        == 0.4.*\r\n\r\n  hs-source-dirs:      src\r\n  default-language:    Haskell2010\r\n  ghc-options:         -Wall\r\n\r\ntest-suite spec\r\n  type:                exitcode-stdio-1.0\r\n  main-is:             Spec.hs\r\n  other-modules:       BoundedEnumIxSpec\r\n                       DerivingViaSpec\r\n                       EqSpec\r\n                       FunctorSpec\r\n                       OrdSpec\r\n                       ReadSpec\r\n                       ShowSpec\r\n                       GH6Spec\r\n                       GH24Spec\r\n                       GH27Spec\r\n                       GH31Spec\r\n\r\n                       Types.EqOrd\r\n                       Types.ReadShow\r\n  build-depends:       base-compat         >= 0.8.1  && < 1\r\n                     , base-orphans        >= 0.5    && < 1\r\n                     , deriving-compat\r\n                     , hspec               >= 1.8\r\n                     , QuickCheck          >= 2      && < 3\r\n                     , tagged              >= 0.7    && < 1\r\n                     , template-haskell    >= 2.5    && < 2.20\r\n                     , void                >= 0.5.10 && < 1\r\n  build-tool-depends:  hspec-discover:hspec-discover >= 1.8\r\n\r\n  if flag(base-4-9)\r\n    build-depends:     base                >= 4.9 && < 5\r\n    cpp-options:       \"-DNEW_FUNCTOR_CLASSES\"\r\n  else\r\n    build-depends:     base                >= 4.3 && < 4.9\r\n\r\n  if flag(new-functor-classes)\r\n    build-depends:     transformers        (>= 0.2 && < 0.4) || (>= 0.5 && < 0.7)\r\n                     , transformers-compat >= 0.5\r\n    cpp-options:       \"-DNEW_FUNCTOR_CLASSES\"\r\n  else\r\n    build-depends:     transformers        == 0.4.*\r\n\r\n  hs-source-dirs:      tests\r\n  default-language:    Haskell2010\r\n  ghc-options:         -Wall -threaded -rtsopts\r\n  if impl(ghc >= 8.6)\r\n    ghc-options:       -Wno-star-is-type\r\n  if impl(ghc >= 9.0)\r\n    ghc-options:       -fenable-th-splice-warnings\r\n";
    }