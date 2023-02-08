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
      identifier = { name = "th-orphans"; version = "0.13.11"; };
      license = "BSD-3-Clause";
      copyright = "(c) Matt Morrow, Michael Sloan, Ryan Scott";
      maintainer = "Ryan Scott <ryan.gl.scott@gmail.com>";
      author = "Matt Morrow, Michael Sloan, Ryan Scott";
      homepage = "";
      url = "";
      synopsis = "Orphan instances for TH datatypes";
      description = "Orphan instances for TH datatypes.  In particular, instances\nfor Ord and Lift, as well as a few missing Show / Eq.  These\ninstances used to live in haskell-src-meta, and that's where\nthe version number started.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = (([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."th-compat" or (errorHandler.buildDepError "th-compat"))
          (hsPkgs."th-lift" or (errorHandler.buildDepError "th-lift"))
          (hsPkgs."th-reify-many" or (errorHandler.buildDepError "th-reify-many"))
          (hsPkgs."th-lift-instances" or (errorHandler.buildDepError "th-lift-instances"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          ] ++ (pkgs.lib).optionals (!(compiler.isGhc && (compiler.version).ge "8.0")) [
          (hsPkgs."fail" or (errorHandler.buildDepError "fail"))
          (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
          ]) ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).lt "7.10") (hsPkgs."generic-deriving" or (errorHandler.buildDepError "generic-deriving"))) ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).ge "7.2" && (compiler.isGhc && (compiler.version).lt "7.6")) (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"));
        buildable = true;
        };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
            (hsPkgs."th-lift" or (errorHandler.buildDepError "th-lift"))
            (hsPkgs."th-orphans" or (errorHandler.buildDepError "th-orphans"))
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
      url = "http://hackage.haskell.org/package/th-orphans-0.13.11.tar.gz";
      sha256 = "6e4a5b6e24a615cb6eb0849382ff04204757be36b82fdc0431ccd506a5f6f5d4";
      });
    }) // {
    package-description-override = "name:               th-orphans\nversion:            0.13.11\ncabal-version:      >= 1.10\nbuild-type:         Simple\nlicense:            BSD3\nlicense-file:       LICENSE\ncategory:           Template Haskell\nauthor:             Matt Morrow, Michael Sloan, Ryan Scott\ncopyright:          (c) Matt Morrow, Michael Sloan, Ryan Scott\nmaintainer:         Ryan Scott <ryan.gl.scott@gmail.com>\nbug-reports:        https://github.com/mgsloan/th-orphans/issues\nstability:          experimental\ntested-with:        GHC == 7.0.4\n                  , GHC == 7.2.2\n                  , GHC == 7.4.2\n                  , GHC == 7.6.3\n                  , GHC == 7.8.4\n                  , GHC == 7.10.3\n                  , GHC == 8.0.2\n                  , GHC == 8.2.2\n                  , GHC == 8.4.4\n                  , GHC == 8.6.5\n                  , GHC == 8.8.4\n                  , GHC == 8.10.2\nsynopsis:           Orphan instances for TH datatypes\ndescription:        Orphan instances for TH datatypes.  In particular, instances\n                    for Ord and Lift, as well as a few missing Show / Eq.  These\n                    instances used to live in haskell-src-meta, and that's where\n                    the version number started.\nextra-source-files: CHANGELOG.md, README.md\n\nlibrary\n  build-depends:      base >= 4.3 && < 5,\n                      template-haskell < 2.18,\n                      th-compat >= 0.1 && < 0.2,\n                      -- https://github.com/mboes/th-lift/issues/14\n                      th-lift >= 0.7.1,\n                      th-reify-many >= 0.1 && < 0.2,\n                      th-lift-instances,\n                      mtl\n\n  if !impl(ghc >= 8.0)\n    build-depends:    fail == 4.9.*,\n                      semigroups >= 0.18.5  && < 0.20\n\n  -- Use TH to derive Generics instances instead of DeriveGeneric, for < 7.10\n  if impl(ghc < 7.10)\n    build-depends:    generic-deriving >= 1.9\n\n  -- Prior to GHC 7.6, GHC generics lived in ghc-prim\n  if impl(ghc >= 7.2) && impl(ghc < 7.6)\n    build-depends:    ghc-prim\n\n  hs-source-dirs:     src\n  ghc-options:        -Wall\n  if impl(ghc >= 8.6)\n    ghc-options:      -Wno-star-is-type\n  exposed-modules:    Language.Haskell.TH.Instances\n  other-modules:      Language.Haskell.TH.Instances.Internal\n  default-language:   Haskell2010\n\ntest-suite test\n  type:               exitcode-stdio-1.0\n  hs-source-dirs:     test\n  main-is:            Spec.hs\n  other-modules:      TestUtil\n  build-depends:      base,\n                      bytestring,\n                      ghc-prim,\n                      hspec,\n                      template-haskell,\n                      th-lift,\n                      th-orphans\n  build-tool-depends: hspec-discover:hspec-discover\n  default-language:   Haskell2010\n\nsource-repository head\n  type:     git\n  location: https://github.com/mgsloan/th-orphans\n";
    }