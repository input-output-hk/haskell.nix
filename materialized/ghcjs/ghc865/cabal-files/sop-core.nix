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
      identifier = { name = "sop-core"; version = "0.5.0.1"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "andres@well-typed.com";
      author = "Edsko de Vries <edsko@well-typed.com>, Andres Löh <andres@well-typed.com>";
      homepage = "";
      url = "";
      synopsis = "True Sums of Products";
      description = "Implementation of n-ary sums and n-ary products.\n\nThe module \"Data.SOP\" is the main module of this library and contains\nmore detailed documentation.\n\nThe main use case of this package is to serve as the core of\n@<https://hackage.haskell.org/package/generics-sop generics-sop>@.\n\nA detailed description of the ideas behind this library is provided by\nthe paper:\n\n* Edsko de Vries and Andres Löh.\n<http://www.andres-loeh.de/TrueSumsOfProducts True Sums of Products>.\nWorkshop on Generic Programming (WGP) 2014.\n";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/sop-core-0.5.0.1.tar.gz";
      sha256 = "dac367f1608c9bd6c5dd1697e2a30e1b760617023b96e1df7d44c6c017999db0";
      });
    }) // {
    package-description-override = "name:                sop-core\nversion:             0.5.0.1\nx-revision: 1\nsynopsis:            True Sums of Products\ndescription:\n  Implementation of n-ary sums and n-ary products.\n  .\n  The module \"Data.SOP\" is the main module of this library and contains\n  more detailed documentation.\n  .\n  The main use case of this package is to serve as the core of\n  @<https://hackage.haskell.org/package/generics-sop generics-sop>@.\n  .\n  A detailed description of the ideas behind this library is provided by\n  the paper:\n  .\n    * Edsko de Vries and Andres Löh.\n      <http://www.andres-loeh.de/TrueSumsOfProducts True Sums of Products>.\n      Workshop on Generic Programming (WGP) 2014.\n  .\nlicense:             BSD3\nlicense-file:        LICENSE\nauthor:              Edsko de Vries <edsko@well-typed.com>, Andres Löh <andres@well-typed.com>\nmaintainer:          andres@well-typed.com\ncategory:            Data\nbuild-type:          Simple\ncabal-version:       >=1.10\nextra-source-files:  CHANGELOG.md doctest.sh\ntested-with:         GHC == 8.0.2, GHC == 8.2.2, GHC == 8.4.4, GHC == 8.6.5, GHC == 8.8.2, GHC == 8.10.1\n\nsource-repository head\n  type:                git\n  location:            https://github.com/well-typed/generics-sop\n\nlibrary\n  exposed-modules:     Data.SOP\n                       Data.SOP.Dict\n                       -- exposed via Data.SOP:\n                       Data.SOP.BasicFunctors\n                       Data.SOP.Classes\n                       Data.SOP.Constraint\n                       Data.SOP.NP\n                       Data.SOP.NS\n                       Data.SOP.Sing\n  build-depends:       base                 >= 4.9  && < 4.16,\n                       deepseq              >= 1.3  && < 1.5\n  hs-source-dirs:      src\n  default-language:    Haskell2010\n  ghc-options:         -Wall\n  default-extensions:  CPP\n                       ScopedTypeVariables\n                       TypeFamilies\n                       RankNTypes\n                       TypeOperators\n                       GADTs\n                       ConstraintKinds\n                       MultiParamTypeClasses\n                       TypeSynonymInstances\n                       FlexibleInstances\n                       FlexibleContexts\n                       DeriveFunctor\n                       DeriveFoldable\n                       DeriveTraversable\n                       DefaultSignatures\n                       KindSignatures\n                       DataKinds\n                       FunctionalDependencies\n\n  if impl(ghc <8.2)\n    default-extensions: AutoDeriveTypeable\n\n  -- if impl(ghc >= 8.6)\n  --   default-extensions: NoStarIsType\n  other-extensions:    PolyKinds\n                       UndecidableInstances\n                       DeriveGeneric\n                       StandaloneDeriving\n                       EmptyCase\n                       UndecidableSuperClasses\n                       BangPatterns\n";
    }