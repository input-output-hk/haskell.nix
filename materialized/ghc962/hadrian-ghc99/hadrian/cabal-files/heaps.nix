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
      identifier = { name = "heaps"; version = "0.4"; };
      license = "BSD-3-Clause";
      copyright = "(c) 2010-2015 Edward A. Kmett";
      maintainer = "Edward A. Kmett <ekmett@gmail.com>";
      author = "Edward A. Kmett";
      homepage = "http://github.com/ekmett/heaps/";
      url = "";
      synopsis = "Asymptotically optimal Brodal/Okasaki heaps.";
      description = "Asymptotically optimal Brodal\\/Okasaki bootstrapped skew-binomial heaps from the paper <http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.48.973 \"Optimal Purely Functional Priority Queues\">, extended with a 'Foldable' interface.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [ (hsPkgs."base" or (errorHandler.buildDepError "base")) ];
        buildable = true;
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/heaps-0.4.tar.gz";
      sha256 = "89329df8b95ae99ef272e41e7a2d0fe2f1bb7eacfcc34bc01664414b33067cfd";
    });
  }) // {
    package-description-override = "name:           heaps\nversion:        0.4\nlicense:        BSD3\nlicense-file:   LICENSE\nauthor:         Edward A. Kmett\nmaintainer:     Edward A. Kmett <ekmett@gmail.com>\nstability:      experimental\nhomepage:       http://github.com/ekmett/heaps/\nbug-reports:    http://github.com/ekmett/heaps/issues\ncategory:       Data Structures\nsynopsis:       Asymptotically optimal Brodal/Okasaki heaps.\ndescription:    Asymptotically optimal Brodal\\/Okasaki bootstrapped skew-binomial heaps from the paper <http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.48.973 \"Optimal Purely Functional Priority Queues\">, extended with a 'Foldable' interface.\ncopyright:      (c) 2010-2015 Edward A. Kmett\ntested-with:    GHC == 7.0.4\n              , GHC == 7.2.2\n              , GHC == 7.4.2\n              , GHC == 7.6.3\n              , GHC == 7.8.4\n              , GHC == 7.10.3\n              , GHC == 8.0.2\n              , GHC == 8.2.2\n              , GHC == 8.4.4\n              , GHC == 8.6.5\n              , GHC == 8.8.3\n              , GHC == 8.10.1\nbuild-type:     Simple\ncabal-version:  >=1.10\nextra-source-files:\n  .gitignore\n  .hlint.yaml\n  CHANGELOG.markdown\n  README.markdown\n\nsource-repository head\n  type: git\n  location: git://github.com/ekmett/heaps.git\n\nlibrary\n  exposed-modules: Data.Heap\n  build-depends:\n    base >= 4 && < 6\n  hs-source-dirs: src\n  ghc-options: -O2 -Wall\n  default-language: Haskell2010\n";
  }