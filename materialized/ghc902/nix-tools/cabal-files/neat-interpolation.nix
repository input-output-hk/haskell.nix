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
      identifier = { name = "neat-interpolation"; version = "0.5.1.3"; };
      license = "MIT";
      copyright = "(c) 2013, Nikita Volkov";
      maintainer = "Nikita Volkov <nikita.y.volkov@mail.ru>";
      author = "Nikita Volkov <nikita.y.volkov@mail.ru>";
      homepage = "https://github.com/nikita-volkov/neat-interpolation";
      url = "";
      synopsis = "A quasiquoter for neat and simple multiline text interpolation";
      description = "A quasiquoter for producing Text values with support for\na simple interpolation of input values.\nIt removes the excessive indentation from the input and\naccurately manages the indentation of all lines of the interpolated variables.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."megaparsec" or (errorHandler.buildDepError "megaparsec"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          ];
        buildable = true;
        };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."neat-interpolation" or (errorHandler.buildDepError "neat-interpolation"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."quickcheck-instances" or (errorHandler.buildDepError "quickcheck-instances"))
            (hsPkgs."rerebase" or (errorHandler.buildDepError "rerebase"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/neat-interpolation-0.5.1.3.tar.gz";
      sha256 = "1fab6182a280d48ffffccb7fbe09c4f5bf67f4b29df5d6651d813f1336636507";
      });
    }) // {
    package-description-override = "name: neat-interpolation\nversion: 0.5.1.3\nsynopsis: A quasiquoter for neat and simple multiline text interpolation\ndescription:\n  A quasiquoter for producing Text values with support for\n  a simple interpolation of input values.\n  It removes the excessive indentation from the input and\n  accurately manages the indentation of all lines of the interpolated variables.\ncategory: String, QuasiQuotes\nlicense: MIT\nlicense-file: LICENSE\ncopyright: (c) 2013, Nikita Volkov\nauthor: Nikita Volkov <nikita.y.volkov@mail.ru>\nmaintainer: Nikita Volkov <nikita.y.volkov@mail.ru>\nhomepage: https://github.com/nikita-volkov/neat-interpolation\nbug-reports: https://github.com/nikita-volkov/neat-interpolation/issues\nbuild-type: Simple\ncabal-version: >=1.10\nextra-source-files: CHANGELOG.md\n\nsource-repository head\n  type: git\n  location: git://github.com/nikita-volkov/neat-interpolation.git\n\nlibrary\n  hs-source-dirs: library\n  default-extensions: BangPatterns, BinaryLiterals, ConstraintKinds, DataKinds, DefaultSignatures, DeriveDataTypeable, DeriveFoldable, DeriveFunctor, DeriveGeneric, DeriveTraversable, DuplicateRecordFields, EmptyDataDecls, FlexibleContexts, FlexibleInstances, FunctionalDependencies, GADTs, GeneralizedNewtypeDeriving, LambdaCase, LiberalTypeSynonyms, MagicHash, MultiParamTypeClasses, MultiWayIf, NoImplicitPrelude, NoMonomorphismRestriction, OverloadedLists, OverloadedStrings, PatternGuards, PatternSynonyms, ParallelListComp, QuasiQuotes, RankNTypes, RecordWildCards, ScopedTypeVariables, StandaloneDeriving, StrictData, TemplateHaskell, TupleSections, TypeApplications, TypeFamilies, TypeOperators, UnboxedTuples\n  default-language: Haskell2010\n  exposed-modules:\n    NeatInterpolation\n  other-modules:\n    NeatInterpolation.Parsing\n    NeatInterpolation.Prelude\n    NeatInterpolation.String\n  build-depends:\n    base >=4.9 && <5,\n    megaparsec >=7 && <10,\n    template-haskell >=2.8 && <3,\n    text >=1 && <3\n\ntest-suite test\n  type: exitcode-stdio-1.0\n  hs-source-dirs: test\n  default-extensions: BangPatterns, BinaryLiterals, ConstraintKinds, DataKinds, DefaultSignatures, DeriveDataTypeable, DeriveFoldable, DeriveFunctor, DeriveGeneric, DeriveTraversable, DuplicateRecordFields, EmptyDataDecls, FlexibleContexts, FlexibleInstances, FunctionalDependencies, GADTs, GeneralizedNewtypeDeriving, LambdaCase, LiberalTypeSynonyms, MagicHash, MultiParamTypeClasses, MultiWayIf, NoImplicitPrelude, NoMonomorphismRestriction, OverloadedLists, OverloadedStrings, PatternGuards, PatternSynonyms, ParallelListComp, QuasiQuotes, RankNTypes, RecordWildCards, ScopedTypeVariables, StandaloneDeriving, StrictData, TemplateHaskell, TupleSections, TypeApplications, TypeFamilies, TypeOperators, UnboxedTuples\n  default-language: Haskell2010\n  main-is: Main.hs\n  build-depends:\n    neat-interpolation,\n    QuickCheck >=2.13 && <3,\n    quickcheck-instances >=0.3.22 && <0.4,\n    rerebase <2,\n    tasty >=1.2.3 && <2,\n    tasty-hunit >=0.10.0.2 && <0.11,\n    tasty-quickcheck >=0.10.1 && <0.11\n";
    }