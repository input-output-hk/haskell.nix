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
      identifier = { name = "th-reify-many"; version = "0.1.9"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Michael Sloan <mgsloan at gmail>";
      author = "Michael Sloan";
      homepage = "http://github.com/mgsloan/th-reify-many";
      url = "";
      synopsis = "Recurseively reify template haskell datatype info";
      description = "@th-reify-many@ provides functions for recursively reifying top\nlevel declarations.  The main intended use case is for enumerating\nthe names of datatypes reachable from an initial datatype, and\npassing these names to some function which generates instances.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."safe" or (errorHandler.buildDepError "safe"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."th-expand-syns" or (errorHandler.buildDepError "th-expand-syns"))
          ];
        buildable = true;
        };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."th-reify-many" or (errorHandler.buildDepError "th-reify-many"))
            (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/th-reify-many-0.1.9.tar.gz";
      sha256 = "f889dd029d5ab191ace99fe595f363c60314d536e61c8c58f6167f1a9d29ae43";
      });
    }) // {
    package-description-override = "name:              th-reify-many\nversion:           0.1.9\nsynopsis:          Recurseively reify template haskell datatype info\n\ndescription:       @th-reify-many@ provides functions for recursively reifying top\n                   level declarations.  The main intended use case is for enumerating\n                   the names of datatypes reachable from an initial datatype, and\n                   passing these names to some function which generates instances.\nlicense:           BSD3\nlicense-file:      LICENSE\nauthor:            Michael Sloan\nmaintainer:        Michael Sloan <mgsloan at gmail>\nhomepage:          http://github.com/mgsloan/th-reify-many\nbug-reports:       http://github.com/mgsloan/th-reify-many/issues\ncategory:          Template Haskell\nstability:         Experimental\ncabal-version:     >= 1.10\nbuild-type:        Simple\nsource-repository head\n  type: git\n  location: git://github.com/mgsloan/th-reify-many\n\nlibrary\n    hs-source-dirs:  src\n    ghc-options:     -Wall\n    exposed-modules: Language.Haskell.TH.ReifyMany\n                     Language.Haskell.TH.ReifyMany.Internal\n    -- Note: these lack version bounds because this library builds\n    -- with the earliest and latest versions of all dependencies\n    -- except for template-haskell.\n    build-depends: base >= 4 && < 5\n                 , containers\n                 , mtl\n                 , safe\n                 , template-haskell >= 2.5.0.0\n                 , th-expand-syns\n    default-language: Haskell2010\n\ntest-suite test\n    type:             exitcode-stdio-1.0\n    hs-source-dirs:   tests\n    main-is:          Main.hs\n    build-depends:    base,\n                      th-reify-many,\n                      template-haskell\n    default-language: Haskell2010\n";
    }