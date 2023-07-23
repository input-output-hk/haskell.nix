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
    flags = { semigroupoids = true; };
    package = {
      specVersion = "1.10";
      identifier = { name = "semialign"; version = "1.2.0.1"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Oleg Grenrus <oleg.grenrus@iki.fi>";
      author = "C. McCann, Oleg Grenrus";
      homepage = "https://github.com/haskellari/these";
      url = "";
      synopsis = "Align and Zip type-classes from the common Semialign ancestor.";
      description = "The major use of @These@ of this is provided by the @align@ member of\n@Semialign@ class, representing a generalized notion of \"zipping with padding\"\nthat combines structures without truncating to the size of the smaller input.\n\nIt turns out that @zip@ operation fits well the @Semialign@ class,\nforming lattice-like structure.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = (((([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."these" or (errorHandler.buildDepError "these"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."indexed-traversable" or (errorHandler.buildDepError "indexed-traversable"))
          (hsPkgs."indexed-traversable-instances" or (errorHandler.buildDepError "indexed-traversable-instances"))
          (hsPkgs."tagged" or (errorHandler.buildDepError "tagged"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          ] ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8.2")) (hsPkgs."bifunctors" or (errorHandler.buildDepError "bifunctors"))) ++ (pkgs.lib).optionals (!(compiler.isGhc && (compiler.version).ge "8.0")) ([
          (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."transformers-compat" or (errorHandler.buildDepError "transformers-compat"))
          ] ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).ge "7.10") (hsPkgs."transformers" or (errorHandler.buildDepError "transformers")))) ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "7.10")) (hsPkgs."void" or (errorHandler.buildDepError "void"))) ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).lt "7.5") (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))) ++ (pkgs.lib).optional (flags.semigroupoids) (hsPkgs."semigroupoids" or (errorHandler.buildDepError "semigroupoids"));
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/semialign-1.2.0.1.tar.gz";
      sha256 = "d900697041ae4b0cca3243273a2b3e80bcf74d937405d6a5ff34dc33ee952132";
      });
    }) // {
    package-description-override = "cabal-version:      >=1.10\nname:               semialign\nversion:            1.2.0.1\nx-revision:         3\nsynopsis:\n  Align and Zip type-classes from the common Semialign ancestor.\n\nhomepage:           https://github.com/haskellari/these\nlicense:            BSD3\nlicense-file:       LICENSE\nauthor:             C. McCann, Oleg Grenrus\nmaintainer:         Oleg Grenrus <oleg.grenrus@iki.fi>\ncategory:           Data, These\nbuild-type:         Simple\nextra-source-files: CHANGELOG.md\ndescription:\n  The major use of @These@ of this is provided by the @align@ member of\n  @Semialign@ class, representing a generalized notion of \"zipping with padding\"\n  that combines structures without truncating to the size of the smaller input.\n  .\n  It turns out that @zip@ operation fits well the @Semialign@ class,\n  forming lattice-like structure.\n\ntested-with:\n    GHC ==7.4.2\n     || ==7.6.3\n     || ==7.8.4\n     || ==7.10.3\n     || ==8.0.2\n     || ==8.2.2\n     || ==8.4.4\n     || ==8.6.5\n     || ==8.8.4\n     || ==8.10.4\n     || ==9.0.2\n     || ==9.2.4\n     || ==9.4.1\n  , GHCJS ==8.4\n\nsource-repository head\n  type:     git\n  location: https://github.com/haskellari/these.git\n  subdir:   semialign\n\nflag semigroupoids\n  description: Build with semigroupoids dependency\n  manual:      True\n  default:     True\n\nlibrary\n  default-language: Haskell2010\n  ghc-options:      -Wall\n\n  if impl(ghc >=8.0)\n    ghc-options: -Wno-trustworthy-safe\n  if impl(ghc >=9.2)\n    ghc-options: -Wno-noncanonical-monoid-instances\n\n  hs-source-dirs:   src\n  exposed-modules:\n    Data.Align\n    Data.Crosswalk\n    Data.Semialign\n    Data.Semialign.Indexed\n    Data.Zip\n\n  other-modules:    Data.Semialign.Internal\n\n  -- ghc boot libs\n  build-depends:\n      base          >=4.5.1.0 && <4.18\n    , containers    >=0.4.2.1 && <0.7\n    , transformers  >=0.3.0.0 && <0.7\n\n  -- These\n  build-depends:    these >=1.1.1.1 && <1.2\n\n  -- other dependencies\n  build-depends:\n      hashable                       >=1.2.7.0  && <1.5\n    , indexed-traversable            >=0.1.1    && <0.2\n    , indexed-traversable-instances  >=0.1      && <0.2\n    , tagged                         >=0.8.6    && <0.9\n    , unordered-containers           >=0.2.8.0  && <0.3\n    , vector                         >=0.12.0.2 && <0.14\n\n  -- base shims\n  if !impl(ghc >=8.2)\n    build-depends: bifunctors >=5.5.4 && <5.6\n\n  if !impl(ghc >=8.0)\n    build-depends:\n        semigroups           >=0.18.5  && <0.21\n      , transformers         >=0.3.0.0 && <0.7\n      , transformers-compat  >=0.6.5   && <0.8\n\n    -- Ensure Data.Functor.Classes is always available\n    if impl(ghc >=7.10)\n      build-depends: transformers >=0.4.2.0\n\n  if !impl(ghc >=7.10)\n    build-depends: void >=0.7.3 && <0.8\n\n  if impl(ghc <7.5)\n    build-depends: ghc-prim\n\n  if flag(semigroupoids)\n    build-depends: semigroupoids >=5.3.2 && <5.4\n";
    }