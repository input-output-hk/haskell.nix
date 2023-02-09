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
      identifier = { name = "lens-family-core"; version = "2.1.2"; };
      license = "BSD-3-Clause";
      copyright = "Copyright (C) 2012,2013,2014,2017,2018,2019 Russell O'Connor";
      maintainer = "Russell O'Connor <roconnor@theorem.ca>";
      author = "Russell O'Connor";
      homepage = "";
      url = "";
      synopsis = "Haskell 2022 Lens Families";
      description = "This package provides first class(†) functional references in Van Laarhoven style supporting the following optics:\n\n* Lenses (view, over)\n\n* Traversals (toListOf, matching, over)\n\n* Setters (over)\n\n* Grates (zipWithOf, under, review)\n\n* Resetters (under)\n\n* Adapters (view, review)\n\n* Grids (toListOf, over / under, review)\n\n* Prisms (matching, over / under, review)\n\n* Getters (view)\n\n* Folders (toListOf)\n\n* Reviewers (review)\n\n(†) For optimal first-class support use the @lens-family@ package with rank 2 / rank N polymorphism.\n\"Lens.Family.Clone\" allows for first-class support of lenses and traversals for those who cannot support rank 2 polymorphism.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/lens-family-core-2.1.2.tar.gz";
      sha256 = "1b5a997276c8b77a96f99f48b95b204d34f3bb84fa3691747cd30bc8c76873b6";
      });
    }) // {
    package-description-override = "name:               lens-family-core\ncategory:           Data, Lenses\nversion:            2.1.2\nlicense:            BSD3\ncabal-version:      >= 1.10\nlicense-file:       LICENSE\nauthor:             Russell O'Connor\nmaintainer:         Russell O'Connor <roconnor@theorem.ca>\nstability:          experimental\ncopyright:          Copyright (C) 2012,2013,2014,2017,2018,2019 Russell O'Connor\nsynopsis:           Haskell 2022 Lens Families\nbuild-type:         Simple\nextra-source-files: CHANGELOG\ndescription:        This package provides first class(†) functional references in Van Laarhoven style supporting the following optics:\n                    .\n                    * Lenses (view, over)\n                    .\n                    * Traversals (toListOf, matching, over)\n                    .\n                    * Setters (over)\n                    .\n                    * Grates (zipWithOf, under, review)\n                    .\n                    * Resetters (under)\n                    .\n                    * Adapters (view, review)\n                    .\n                    * Grids (toListOf, over / under, review)\n                    .\n                    * Prisms (matching, over / under, review)\n                    .\n                    * Getters (view)\n                    .\n                    * Folders (toListOf)\n                    .\n                    * Reviewers (review)\n                    .\n                    (†) For optimal first-class support use the @lens-family@ package with rank 2 / rank N polymorphism.\n                    \"Lens.Family.Clone\" allows for first-class support of lenses and traversals for those who cannot support rank 2 polymorphism.\n\nsource-repository head\n  type:     darcs\n  location: https://hub.darcs.net/roconnor/lens-family\n\nlibrary\n  default-language:   Haskell2010\n  build-depends:\n    base                 >= 4.11    && < 5,\n    containers           >= 0.5.8   && < 0.7,\n    transformers         >= 0.3.0   && < 0.7\n\n  exposed-modules:\n    Lens.Family.Unchecked\n    Lens.Family.Clone\n    Lens.Family\n    Lens.Family.Stock\n    Lens.Family.State.Lazy\n    Lens.Family.State.Strict\n    Lens.Family.State\n  other-modules:\n    Lens.Family.Identical\n    Lens.Family.Phantom\n    Lens.Family.State.Zoom\n\n  ghc-options:      -Wall\n\n  hs-source-dirs:\n                    src\n\n";
    }