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
      identifier = { name = "generics-sop"; version = "0.5.1.0"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "andres@well-typed.com";
      author = "Edsko de Vries <edsko@well-typed.com>, Andres Löh <andres@well-typed.com>";
      homepage = "";
      url = "";
      synopsis = "Generic Programming using True Sums of Products";
      description = "A library to support the definition of generic functions.\nDatatypes are viewed in a uniform, structured way:\nthe choice between constructors is represented using an n-ary\nsum, and the arguments of each constructor are represented using\nan n-ary product.\n\nThe module \"Generics.SOP\" is the main module of this library and contains\nmore detailed documentation.\n\nSince version 0.4.0.0, this package is now based on\n@<https://hackage.haskell.org/package/sop-core sop-core>@. The core package\ncontains all the functionality of n-ary sums and products, whereas this\npackage provides the datatype-generic programming support on top.\n\nExamples of using this library are provided by the following\npackages:\n\n* @<https://hackage.haskell.org/package/basic-sop basic-sop>@ basic examples,\n\n* @<https://hackage.haskell.org/package/pretty-sop pretty-sop>@ generic pretty printing,\n\n* @<https://hackage.haskell.org/package/lens-sop lens-sop>@ generically computed lenses,\n\n* @<https://hackage.haskell.org/package/json-sop json-sop>@ generic JSON conversions.\n\nA detailed description of the ideas behind this library is provided by\nthe paper:\n\n* Edsko de Vries and Andres Löh.\n<http://www.andres-loeh.de/TrueSumsOfProducts True Sums of Products>.\nWorkshop on Generic Programming (WGP) 2014.\n";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."sop-core" or (errorHandler.buildDepError "sop-core"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."th-abstraction" or (errorHandler.buildDepError "th-abstraction"))
          (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
          ];
        buildable = true;
        };
      tests = {
        "generics-sop-examples" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."generics-sop" or (errorHandler.buildDepError "generics-sop"))
            ];
          buildable = true;
          };
        };
      benchmarks = {
        "generics-sop-bench" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."generics-sop" or (errorHandler.buildDepError "generics-sop"))
            (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/generics-sop-0.5.1.0.tar.gz";
      sha256 = "eac657aa743282a81e03438744ac93053d989b829f6b3ca4837c70bbca041f3c";
      });
    }) // {
    package-description-override = "name:                generics-sop\r\nversion:             0.5.1.0\r\nx-revision: 1\r\nsynopsis:            Generic Programming using True Sums of Products\r\ndescription:\r\n  A library to support the definition of generic functions.\r\n  Datatypes are viewed in a uniform, structured way:\r\n  the choice between constructors is represented using an n-ary\r\n  sum, and the arguments of each constructor are represented using\r\n  an n-ary product.\r\n  .\r\n  The module \"Generics.SOP\" is the main module of this library and contains\r\n  more detailed documentation.\r\n  .\r\n  Since version 0.4.0.0, this package is now based on\r\n  @<https://hackage.haskell.org/package/sop-core sop-core>@. The core package\r\n  contains all the functionality of n-ary sums and products, whereas this\r\n  package provides the datatype-generic programming support on top.\r\n  .\r\n  Examples of using this library are provided by the following\r\n  packages:\r\n  .\r\n    * @<https://hackage.haskell.org/package/basic-sop basic-sop>@ basic examples,\r\n  .\r\n    * @<https://hackage.haskell.org/package/pretty-sop pretty-sop>@ generic pretty printing,\r\n  .\r\n    * @<https://hackage.haskell.org/package/lens-sop lens-sop>@ generically computed lenses,\r\n  .\r\n    * @<https://hackage.haskell.org/package/json-sop json-sop>@ generic JSON conversions.\r\n  .\r\n  A detailed description of the ideas behind this library is provided by\r\n  the paper:\r\n  .\r\n    * Edsko de Vries and Andres Löh.\r\n      <http://www.andres-loeh.de/TrueSumsOfProducts True Sums of Products>.\r\n      Workshop on Generic Programming (WGP) 2014.\r\n  .\r\nlicense:             BSD3\r\nlicense-file:        LICENSE\r\nauthor:              Edsko de Vries <edsko@well-typed.com>, Andres Löh <andres@well-typed.com>\r\nmaintainer:          andres@well-typed.com\r\ncategory:            Generics\r\nbuild-type:          Simple\r\ncabal-version:       >=1.10\r\nextra-source-files:  CHANGELOG.md doctest.sh\r\ntested-with:         GHC == 8.0.2, GHC == 8.2.2, GHC == 8.4.4, GHC == 8.6.5, GHC == 8.8.2, GHC == 8.10.1\r\n\r\nsource-repository head\r\n  type:                git\r\n  location:            https://github.com/well-typed/generics-sop\r\n\r\nlibrary\r\n  exposed-modules:     Generics.SOP\r\n                       Generics.SOP.GGP\r\n                       Generics.SOP.TH\r\n                       Generics.SOP.Type.Metadata\r\n                       -- exposed via Generics.SOP:\r\n                       Generics.SOP.Instances\r\n                       Generics.SOP.Metadata\r\n                       Generics.SOP.Universe\r\n                       -- re-exported from Data.SOP:\r\n                       Generics.SOP.Dict\r\n                       Generics.SOP.BasicFunctors\r\n                       Generics.SOP.Classes\r\n                       Generics.SOP.Constraint\r\n                       Generics.SOP.NP\r\n                       Generics.SOP.NS\r\n                       Generics.SOP.Sing\r\n  build-depends:       base                 >= 4.9  && < 4.15,\r\n                       sop-core             == 0.5.0.*,\r\n                       template-haskell     >= 2.8  && < 2.17,\r\n                       th-abstraction       >= 0.3  && < 0.5,\r\n                       ghc-prim             >= 0.3  && < 0.7\r\n  hs-source-dirs:      src\r\n  default-language:    Haskell2010\r\n  ghc-options:         -Wall\r\n  default-extensions:  CPP\r\n                       ScopedTypeVariables\r\n                       TypeFamilies\r\n                       RankNTypes\r\n                       TypeOperators\r\n                       GADTs\r\n                       ConstraintKinds\r\n                       MultiParamTypeClasses\r\n                       TypeSynonymInstances\r\n                       FlexibleInstances\r\n                       FlexibleContexts\r\n                       DeriveFunctor\r\n                       DeriveFoldable\r\n                       DeriveTraversable\r\n                       DefaultSignatures\r\n                       KindSignatures\r\n                       DataKinds\r\n                       FunctionalDependencies\r\n\r\n  if impl(ghc <8.2)\r\n    default-extensions: AutoDeriveTypeable\r\n\r\n  -- if impl(ghc >= 8.6)\r\n  --   default-extensions: NoStarIsType\r\n  other-extensions:    PolyKinds\r\n                       UndecidableInstances\r\n                       TemplateHaskell\r\n                       StandaloneDeriving\r\n                       EmptyCase\r\n                       UndecidableSuperClasses\r\n\r\ntest-suite generics-sop-examples\r\n  type:                exitcode-stdio-1.0\r\n  main-is:             Example.hs\r\n  other-modules:       HTransExample\r\n  hs-source-dirs:      test\r\n  default-language:    Haskell2010\r\n  ghc-options:         -Wall\r\n  build-depends:       base                 >= 4.9  && < 5,\r\n                       generics-sop\r\n  other-extensions:    DeriveGeneric\r\n                       EmptyCase\r\n                       TemplateHaskell\r\n                       ConstraintKinds\r\n                       GADTs\r\n                       DataKinds\r\n                       TypeFamilies\r\n                       FlexibleContexts\r\n                       FlexibleInstances\r\n                       PolyKinds\r\n                       DefaultSignatures\r\n                       FunctionalDependencies\r\n                       MultiParamTypeClasses\r\n                       TypeFamilies\r\n\r\nbenchmark generics-sop-bench\r\n  type:                exitcode-stdio-1.0\r\n  main-is:             SOPBench.hs\r\n  other-modules:       SOPBench.Type\r\n                       SOPBench.Roundtrip\r\n                       SOPBench.Eq\r\n                       SOPBench.Show\r\n  hs-source-dirs:      bench\r\n  default-language:    Haskell2010\r\n  ghc-options:         -Wall\r\n  build-depends:       base                 >= 4.6  && < 5,\r\n                       criterion,\r\n                       deepseq,\r\n                       generics-sop,\r\n                       template-haskell\r\n";
    }