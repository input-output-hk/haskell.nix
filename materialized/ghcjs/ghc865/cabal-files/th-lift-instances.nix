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
      identifier = { name = "th-lift-instances"; version = "0.1.18"; };
      license = "BSD-3-Clause";
      copyright = "Copyright (C) 2013-2020 Benno Fünfstück";
      maintainer = "Benno Fünfstück <benno.fuenfstueck@gmail.com>";
      author = "Benno Fünfstück";
      homepage = "http://github.com/bennofs/th-lift-instances/";
      url = "";
      synopsis = "Lift instances for template-haskell for common data types.";
      description = "Most data types in the haskell platform do not have Lift instances.\nThis package provides orphan instances for containers, text, bytestring and vector.\nIt also provides compat instances for older versions of @template-haskell@\n\nNote that <https://hackage.haskell.org/package/th-lift th-lift> package provides\nTemplate Haskell based derivation of @Lift@ instances (when you cannot use @DeriveLift@ extension),\nand <https://hackage.haskell.org/package/th-orphans th-orphans> package provides instances for TH datatypes.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."th-lift" or (errorHandler.buildDepError "th-lift"))
          ];
        buildable = true;
        };
      tests = {
        "tests" = {
          depends = ([
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."th-lift-instances" or (errorHandler.buildDepError "th-lift-instances"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            ] ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "7.10")) (hsPkgs."nats" or (errorHandler.buildDepError "nats"))) ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8.0")) (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"));
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/th-lift-instances-0.1.18.tar.gz";
      sha256 = "bee57c7522e0fefdf8719d4492312883d1a168c6ec4b17befb666fe7f40fdb26";
      });
    }) // {
    package-description-override = "name: th-lift-instances\nversion: 0.1.18\ncabal-version: >=1.10\nbuild-type: Simple\nlicense: BSD3\nlicense-file: LICENSE\ncopyright: Copyright (C) 2013-2020 Benno Fünfstück\nmaintainer: Benno Fünfstück <benno.fuenfstueck@gmail.com>\nstability: experimental\nhomepage: http://github.com/bennofs/th-lift-instances/\nbug-reports: http://github.com/bennofs/th-lift-instances/issues\nsynopsis: Lift instances for template-haskell for common data types.\ndescription:\n    Most data types in the haskell platform do not have Lift instances.\n    This package provides orphan instances for containers, text, bytestring and vector.\n    It also provides compat instances for older versions of @template-haskell@\n    .\n    Note that <https://hackage.haskell.org/package/th-lift th-lift> package provides\n    Template Haskell based derivation of @Lift@ instances (when you cannot use @DeriveLift@ extension),\n    and <https://hackage.haskell.org/package/th-orphans th-orphans> package provides instances for TH datatypes.\ncategory: Template Haskell\nauthor: Benno Fünfstück\nextra-source-files:\n    .ghci\n    .gitignore\n    .travis.yml\n    README.md\n\nsource-repository head\n    type: git\n    location: https://github.com/bennofs/th-lift-instances.git\n\nlibrary\n    exposed-modules:\n        Instances.TH.Lift\n    build-depends:\n        base >=4.3 && <5,\n        template-haskell >=2.5.0.0,\n        containers,\n        vector >= 0.7,\n        text,\n        transformers,\n        bytestring\n\n    -- the dependency is added to avoid diamond orphans problem.\n    -- Without a dependency there could be a plan with th-lift-0.7.x and\n    -- th-lift-instances, which both define instances for same data types.\n    build-depends:\n      th-lift >= 0.8\n\n    default-language: Haskell2010\n    hs-source-dirs: src\n    ghc-options: -Wall -fwarn-tabs\n\n    if impl(ghc >= 8.0)\n      other-extensions: TemplateHaskellQuotes\n    else\n      other-extensions: TemplateHaskell\n\n\ntest-suite tests\n    type: exitcode-stdio-1.0\n    main-is: Main.hs\n    build-depends:\n        base <5,\n        template-haskell,\n        containers,\n        vector >= 0.4,\n        text,\n        bytestring,\n        th-lift-instances,\n        QuickCheck >=2.6\n    if !impl(ghc >= 7.10)\n      build-depends:\n        nats >= 1.1.2 && < 1.2\n    if !impl(ghc >= 8.0)\n      build-depends:\n        -- todo: we need to bump lower bound when new semigroups is released\n        -- with NonEmpty instance\n        semigroups          >= 0.18.5  && < 0.19\n    default-language: Haskell2010\n    other-extensions: TemplateHaskell\n    hs-source-dirs: tests\n    other-modules:\n        Data\n";
    }