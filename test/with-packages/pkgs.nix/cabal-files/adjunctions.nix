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
      identifier = { name = "adjunctions"; version = "4.4.2"; };
      license = "BSD-3-Clause";
      copyright = "Copyright (C) 2011-2014 Edward A. Kmett";
      maintainer = "Edward A. Kmett <ekmett@gmail.com>";
      author = "Edward A. Kmett";
      homepage = "http://github.com/ekmett/adjunctions/";
      url = "";
      synopsis = "Adjunctions and representable functors";
      description = "Adjunctions and representable functors.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."comonad" or (errorHandler.buildDepError "comonad"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."contravariant" or (errorHandler.buildDepError "contravariant"))
          (hsPkgs."distributive" or (errorHandler.buildDepError "distributive"))
          (hsPkgs."free" or (errorHandler.buildDepError "free"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."profunctors" or (errorHandler.buildDepError "profunctors"))
          (hsPkgs."tagged" or (errorHandler.buildDepError "tagged"))
          (hsPkgs."semigroupoids" or (errorHandler.buildDepError "semigroupoids"))
          (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."transformers-compat" or (errorHandler.buildDepError "transformers-compat"))
          (hsPkgs."void" or (errorHandler.buildDepError "void"))
          ] ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).lt "7.6") (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"));
        buildable = true;
        };
      tests = {
        "spec" = {
          depends = [
            (hsPkgs."adjunctions" or (errorHandler.buildDepError "adjunctions"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."distributive" or (errorHandler.buildDepError "distributive"))
            (hsPkgs."generic-deriving" or (errorHandler.buildDepError "generic-deriving"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
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
      url = "http://hackage.haskell.org/package/adjunctions-4.4.2.tar.gz";
      sha256 = "147b5f8db810bca0ea8952ef974982ffc447cecd21f01e1ea1121df77e276518";
      });
    }) // {
    package-description-override = "name:          adjunctions\ncategory:      Data Structures, Adjunctions\nversion:       4.4.2\nlicense:       BSD3\ncabal-version: >= 1.10\nlicense-file:  LICENSE\nauthor:        Edward A. Kmett\nmaintainer:    Edward A. Kmett <ekmett@gmail.com>\nstability:     provisional\nhomepage:      http://github.com/ekmett/adjunctions/\nbug-reports:   http://github.com/ekmett/adjunctions/issues\ncopyright:     Copyright (C) 2011-2014 Edward A. Kmett\nsynopsis:      Adjunctions and representable functors\ndescription:   Adjunctions and representable functors.\nbuild-type:    Simple\nextra-source-files:\n  .gitignore\n  .vim.custom\n  HLint.hs\n  CHANGELOG.markdown\n  README.markdown\ntested-with:   GHC == 7.4.2\n             , GHC == 7.6.3\n             , GHC == 7.8.4\n             , GHC == 7.10.3\n             , GHC == 8.0.2\n             , GHC == 8.2.2\n             , GHC == 8.4.4\n             , GHC == 8.6.5\n             , GHC == 8.8.4\n             , GHC == 8.10.7\n             , GHC == 9.0.2\n             , GHC == 9.2.2\n\nsource-repository head\n  type: git\n  location: git://github.com/ekmett/adjunctions.git\n\nlibrary\n  hs-source-dirs: src\n\n  other-extensions:\n    CPP\n    FunctionalDependencies\n    FlexibleContexts\n    MultiParamTypeClasses\n    Rank2Types\n    UndecidableInstances\n\n  build-depends:\n    array               >= 0.3.0.2 && < 0.7,\n    base                >= 4       && < 5,\n    comonad             >= 4       && < 6,\n    containers          >= 0.3     && < 0.7,\n    contravariant       >= 1       && < 2,\n    distributive        >= 0.5.1   && < 1,\n    free                >= 4       && < 6,\n    mtl                 >= 2.0.1   && < 2.4,\n    profunctors         >= 4       && < 6,\n    tagged              >= 0.7     && < 1,\n    semigroupoids       >= 4       && < 6,\n    semigroups          >= 0.11    && < 1,\n    transformers        >= 0.2     && < 0.7,\n    transformers-compat >= 0.3     && < 1,\n    void                >= 0.5.5.1 && < 1\n\n  if impl(ghc < 7.6)\n    build-depends: ghc-prim\n\n  exposed-modules:\n    Control.Comonad.Representable.Store\n    Control.Comonad.Trans.Adjoint\n    Control.Monad.Representable.Reader\n    Control.Monad.Representable.State\n    Control.Monad.Trans.Adjoint\n    Control.Monad.Trans.Contravariant.Adjoint\n    Control.Monad.Trans.Conts\n    Data.Functor.Adjunction\n    Data.Functor.Contravariant.Adjunction\n    Data.Functor.Contravariant.Rep\n    Data.Functor.Rep\n\n  ghc-options: -Wall\n\n  default-language: Haskell2010\n\n  if impl(ghc >= 8.0)\n    -- See https://ghc.haskell.org/trac/ghc/wiki/Migration/8.0#base-4.9.0.0\n    ghc-options: -Wcompat -Wnoncanonical-monad-instances\n    ghc-options: -Wno-trustworthy-safe -Wno-inline-rule-shadowing\n\n    if !impl(ghc >= 8.8)\n      ghc-options: -Wnoncanonical-monadfail-instances\n\n  if impl(ghc >= 8.6)\n    ghc-options: -Wno-star-is-type\n\ntest-suite spec\n  type:           exitcode-stdio-1.0\n  hs-source-dirs: tests\n\n  build-tool-depends: hspec-discover:hspec-discover >=2 && <3\n  build-depends:\n    adjunctions,\n    base             >= 4     && < 5,\n    distributive     >= 0.5.1 && < 1,\n    generic-deriving >= 1.11  && < 2,\n    hspec            >= 2     && < 3\n\n  main-is: Spec.hs\n  other-modules: GenericsSpec\n\n  ghc-options: -Wall -threaded -rtsopts\n  default-language: Haskell2010\n";
    }