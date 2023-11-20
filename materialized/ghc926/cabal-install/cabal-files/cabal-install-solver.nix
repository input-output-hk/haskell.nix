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
      debug-expensive-assertions = false;
      debug-conflict-sets = false;
      debug-tracetree = false;
      };
    package = {
      specVersion = "2.2";
      identifier = { name = "cabal-install-solver"; version = "3.10.2.1"; };
      license = "BSD-3-Clause";
      copyright = "2003-2023, Cabal Development Team";
      maintainer = "Cabal Development Team <cabal-devel@haskell.org>";
      author = "Cabal Development Team (see AUTHORS file)";
      homepage = "http://www.haskell.org/cabal/";
      url = "";
      synopsis = "The command-line interface for Cabal and Hackage.";
      description = "The solver component used in cabal-install command-line program";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = ([
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."Cabal" or (errorHandler.buildDepError "Cabal"))
          (hsPkgs."Cabal-syntax" or (errorHandler.buildDepError "Cabal-syntax"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."edit-distance" or (errorHandler.buildDepError "edit-distance"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."pretty" or (errorHandler.buildDepError "pretty"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          ] ++ (pkgs.lib).optional (flags.debug-conflict-sets) (hsPkgs."base" or (errorHandler.buildDepError "base"))) ++ (pkgs.lib).optional (flags.debug-tracetree) (hsPkgs."tracetree" or (errorHandler.buildDepError "tracetree"));
        buildable = true;
        };
      tests = {
        "unit-tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."Cabal" or (errorHandler.buildDepError "Cabal"))
            (hsPkgs."Cabal-syntax" or (errorHandler.buildDepError "Cabal-syntax"))
            (hsPkgs."cabal-install-solver" or (errorHandler.buildDepError "cabal-install-solver"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/cabal-install-solver-3.10.2.1.tar.gz";
      sha256 = "91d00728bc4b097da34a7a177a04a4b8eb38c5ae734458346484c432dea5d40a";
      });
    }) // {
    package-description-override = "cabal-version: 2.2\nname:          cabal-install-solver\nversion:       3.10.2.1\nsynopsis:      The command-line interface for Cabal and Hackage.\ndescription:\n  The solver component used in cabal-install command-line program\n\nhomepage:      http://www.haskell.org/cabal/\nbug-reports:   https://github.com/haskell/cabal/issues\nlicense:       BSD-3-Clause\nlicense-file:  LICENSE\nauthor:        Cabal Development Team (see AUTHORS file)\nmaintainer:    Cabal Development Team <cabal-devel@haskell.org>\ncopyright:     2003-2023, Cabal Development Team\ncategory:      Distribution\nbuild-type:    Simple\nExtra-Source-Files:\n  ChangeLog.md\n\nsource-repository head\n  type:     git\n  location: https://github.com/haskell/cabal/\n  subdir:   cabal-install-solver\n\nflag debug-expensive-assertions\n  description: Enable expensive assertions for testing or debugging\n  default:     False\n  manual:      True\n\nflag debug-conflict-sets\n  description: Add additional information to ConflictSets\n  default:     False\n  manual:      True\n\nflag debug-tracetree\n  description: Compile in support for tracetree (used to debug the solver)\n  default:     False\n  manual:      True\n\nlibrary\n  default-language: Haskell2010\n  hs-source-dirs:   src\n  hs-source-dirs:   src-assertion\n  ghc-options:\n    -Wall -Wcompat -Wnoncanonical-monad-instances\n    -fwarn-tabs -fwarn-incomplete-uni-patterns\n\n  if impl(ghc <8.8)\n    ghc-options: -Wnoncanonical-monadfail-instances\n  if impl(ghc >=8.10)\n    ghc-options: -Wunused-packages\n\n  exposed-modules:\n    Distribution.Client.Utils.Assertion\n\n    Distribution.Solver.Compat.Prelude\n    Distribution.Solver.Modular\n    Distribution.Solver.Modular.Assignment\n    Distribution.Solver.Modular.Builder\n    Distribution.Solver.Modular.Configured\n    Distribution.Solver.Modular.ConfiguredConversion\n    Distribution.Solver.Modular.ConflictSet\n    Distribution.Solver.Modular.Cycles\n    Distribution.Solver.Modular.Dependency\n    Distribution.Solver.Modular.Explore\n    Distribution.Solver.Modular.Flag\n    Distribution.Solver.Modular.Index\n    Distribution.Solver.Modular.IndexConversion\n    Distribution.Solver.Modular.LabeledGraph\n    Distribution.Solver.Modular.Linking\n    Distribution.Solver.Modular.Log\n    Distribution.Solver.Modular.Message\n    Distribution.Solver.Modular.MessageUtils\n    Distribution.Solver.Modular.Package\n    Distribution.Solver.Modular.Preference\n    Distribution.Solver.Modular.PSQ\n    Distribution.Solver.Modular.RetryLog\n    Distribution.Solver.Modular.Solver\n    Distribution.Solver.Modular.Tree\n    Distribution.Solver.Modular.Validate\n    Distribution.Solver.Modular.Var\n    Distribution.Solver.Modular.Version\n    Distribution.Solver.Modular.WeightedPSQ\n    Distribution.Solver.Types.ComponentDeps\n    Distribution.Solver.Types.ConstraintSource\n    Distribution.Solver.Types.DependencyResolver\n    Distribution.Solver.Types.Flag\n    Distribution.Solver.Types.InstalledPreference\n    Distribution.Solver.Types.InstSolverPackage\n    Distribution.Solver.Types.LabeledPackageConstraint\n    Distribution.Solver.Types.OptionalStanza\n    Distribution.Solver.Types.PackageConstraint\n    Distribution.Solver.Types.PackageFixedDeps\n    Distribution.Solver.Types.PackageIndex\n    Distribution.Solver.Types.PackagePath\n    Distribution.Solver.Types.PackagePreferences\n    Distribution.Solver.Types.PkgConfigDb\n    Distribution.Solver.Types.Progress\n    Distribution.Solver.Types.ResolverPackage\n    Distribution.Solver.Types.Settings\n    Distribution.Solver.Types.SolverId\n    Distribution.Solver.Types.SolverPackage\n    Distribution.Solver.Types.SourcePackage\n    Distribution.Solver.Types.Variable\n\n  build-depends:\n    , array         >=0.4      && <0.6\n    , base          >=4.10     && <4.20\n    , bytestring    >=0.10.6.0 && <0.13\n    , Cabal         ^>=3.10\n    , Cabal-syntax  ^>=3.10\n    , containers    >=0.5.6.2  && <0.7\n    , edit-distance ^>= 0.2.2\n    , filepath      ^>=1.4.0.0\n    , mtl           >=2.0      && <2.4\n    , pretty        ^>=1.1\n    , transformers  >=0.4.2.0  && <0.7\n\n  if flag(debug-expensive-assertions)\n    cpp-options: -DDEBUG_EXPENSIVE_ASSERTIONS\n\n  if flag(debug-conflict-sets)\n    cpp-options:   -DDEBUG_CONFLICT_SETS\n    build-depends: base >=4.9\n\n  if flag(debug-tracetree)\n    cpp-options:   -DDEBUG_TRACETREE\n    build-depends: tracetree ^>=0.1\n\nTest-Suite unit-tests\n   default-language: Haskell2010\n   ghc-options: -rtsopts -threaded\n\n   type: exitcode-stdio-1.0\n   main-is: UnitTests.hs\n   hs-source-dirs: tests\n   other-modules:\n     UnitTests.Distribution.Solver.Modular.MessageUtils\n\n   build-depends:\n     , base        >= 4.10  && <4.20\n     , Cabal\n     , Cabal-syntax\n     , cabal-install-solver\n     , tasty       >= 1.2.3 && <1.6\n     , tasty-quickcheck\n     , tasty-hunit >= 0.10\n";
    }