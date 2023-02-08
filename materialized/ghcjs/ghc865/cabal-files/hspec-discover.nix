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
      specVersion = "1.12";
      identifier = { name = "hspec-discover"; version = "2.7.8"; };
      license = "MIT";
      copyright = "(c) 2012-2021 Simon Hengel";
      maintainer = "Simon Hengel <sol@typeful.net>";
      author = "Simon Hengel <sol@typeful.net>";
      homepage = "http://hspec.github.io/";
      url = "";
      synopsis = "Automatically discover and run Hspec tests";
      description = "Automatically discover and run Hspec tests\n\n<http://hspec.github.io/hspec-discover.html>";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          ];
        buildable = true;
        };
      exes = {
        "hspec-discover" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."hspec-discover" or (errorHandler.buildDepError "hspec-discover"))
            ];
          buildable = true;
          };
        };
      tests = {
        "spec" = {
          depends = [
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."hspec-discover" or (errorHandler.buildDepError "hspec-discover"))
            (hsPkgs."hspec-meta" or (errorHandler.buildDepError "hspec-meta"))
            ];
          build-tools = [
            (hsPkgs.buildPackages.hspec-meta.components.exes.hspec-meta-discover or (pkgs.buildPackages.hspec-meta-discover or (errorHandler.buildToolDepError "hspec-meta:hspec-meta-discover")))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/hspec-discover-2.7.8.tar.gz";
      sha256 = "c7587dddf1e0ad2d534441aaca00646262b82adcaa625f9697647f4c7cd55e7c";
      });
    }) // {
    package-description-override = "cabal-version: 1.12\n\n-- This file has been generated from package.yaml by hpack version 0.34.3.\n--\n-- see: https://github.com/sol/hpack\n\nname:             hspec-discover\nversion:          2.7.8\nlicense:          MIT\nlicense-file:     LICENSE\ncopyright:        (c) 2012-2021 Simon Hengel\nauthor:           Simon Hengel <sol@typeful.net>\nmaintainer:       Simon Hengel <sol@typeful.net>\nbuild-type:       Simple\ncategory:         Testing\nstability:        experimental\nbug-reports:      https://github.com/hspec/hspec/issues\nhomepage:         http://hspec.github.io/\nsynopsis:         Automatically discover and run Hspec tests\ndescription:      Automatically discover and run Hspec tests\n                  .\n                  <http://hspec.github.io/hspec-discover.html>\nextra-source-files:\n    version.yaml\n    test-data/nested-spec/Foo/Bar/BazSpec.hs\n    test-data/nested-spec/Foo/BarSpec.hs\n    test-data/nested-spec/FooSpec.hs\n    test-data/empty-dir/Foo/Bar/Baz/.placeholder\n\nsource-repository head\n  type: git\n  location: https://github.com/hspec/hspec\n  subdir: hspec-discover\n\nlibrary\n  hs-source-dirs:\n      src\n  ghc-options: -Wall\n  build-depends:\n      base ==4.*\n    , directory\n    , filepath\n  exposed: False\n  exposed-modules:\n      Test.Hspec.Discover.Config\n      Test.Hspec.Discover.Run\n      Test.Hspec.Discover.Sort\n  other-modules:\n      Paths_hspec_discover\n  default-language: Haskell2010\n\nexecutable hspec-discover\n  ghc-options: -Wall\n  hs-source-dirs:\n      driver\n  main-is: hspec-discover.hs\n  build-depends:\n      base ==4.*\n    , directory\n    , filepath\n    , hspec-discover\n  other-modules:\n      Paths_hspec_discover\n  default-language: Haskell2010\n\ntest-suite spec\n  type: exitcode-stdio-1.0\n  ghc-options: -Wall\n  hs-source-dirs:\n      test\n  main-is: Spec.hs\n  other-modules:\n      Helper\n      Test.Hspec.Discover.ConfigSpec\n      Test.Hspec.Discover.RunSpec\n      Test.Hspec.Discover.SortSpec\n      Paths_hspec_discover\n  build-depends:\n      QuickCheck >=2.7\n    , base ==4.*\n    , directory\n    , filepath\n    , hspec-discover\n    , hspec-meta >=2.3.2\n  build-tool-depends:\n      hspec-meta:hspec-meta-discover\n  default-language: Haskell2010\n";
    }