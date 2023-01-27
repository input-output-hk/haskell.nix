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
      identifier = { name = "mono-traversable"; version = "1.0.15.1"; };
      license = "MIT";
      copyright = "";
      maintainer = "michael@snoyman.com";
      author = "Michael Snoyman, John Wiegley, Greg Weber";
      homepage = "https://github.com/snoyberg/mono-traversable#readme";
      url = "";
      synopsis = "Type classes for mapping, folding, and traversing monomorphic containers";
      description = "Please see the README at <https://www.stackage.org/package/mono-traversable>";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."split" or (errorHandler.buildDepError "split"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          (hsPkgs."vector-algorithms" or (errorHandler.buildDepError "vector-algorithms"))
          ] ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).lt "8.0") (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"));
        buildable = true;
        };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."foldl" or (errorHandler.buildDepError "foldl"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."mono-traversable" or (errorHandler.buildDepError "mono-traversable"))
            (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            ];
          buildable = true;
          };
        };
      benchmarks = {
        "sorting" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."gauge" or (errorHandler.buildDepError "gauge"))
            (hsPkgs."mono-traversable" or (errorHandler.buildDepError "mono-traversable"))
            (hsPkgs."mwc-random" or (errorHandler.buildDepError "mwc-random"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/mono-traversable-1.0.15.1.tar.gz";
      sha256 = "c2df5b79ed2f88f2ee313e57c1d591d4463788e20d39e439297eec5ba5835ddf";
      });
    }) // {
    package-description-override = "cabal-version: 1.12\r\n\r\n-- This file has been generated from package.yaml by hpack version 0.31.2.\r\n--\r\n-- see: https://github.com/sol/hpack\r\n--\r\n-- hash: b2ac08c2845dd12213a3bc3c6e01f805bb98a7693a588b0ae313ceadcb5ca592\r\n\r\nname:           mono-traversable\r\nversion:        1.0.15.1\r\nx-revision: 2\r\nsynopsis:       Type classes for mapping, folding, and traversing monomorphic containers\r\ndescription:    Please see the README at <https://www.stackage.org/package/mono-traversable>\r\ncategory:       Data\r\nhomepage:       https://github.com/snoyberg/mono-traversable#readme\r\nbug-reports:    https://github.com/snoyberg/mono-traversable/issues\r\nauthor:         Michael Snoyman, John Wiegley, Greg Weber\r\nmaintainer:     michael@snoyman.com\r\nlicense:        MIT\r\nlicense-file:   LICENSE\r\nbuild-type:     Simple\r\nextra-source-files:\r\n    README.md\r\n    ChangeLog.md\r\n\r\nsource-repository head\r\n  type: git\r\n  location: https://github.com/snoyberg/mono-traversable\r\n\r\nlibrary\r\n  exposed-modules:\r\n      Data.Containers\r\n      Data.MonoTraversable\r\n      Data.MonoTraversable.Unprefixed\r\n      Data.NonNull\r\n      Data.Sequences\r\n  other-modules:\r\n      Paths_mono_traversable\r\n  hs-source-dirs:\r\n      src\r\n  ghc-options: -Wall\r\n  build-depends:\r\n      base >=4.10 && <4.16\r\n        -- 'Option' has been removed from base-4.16\r\n    , bytestring >=0.9\r\n    , containers >=0.5.8\r\n    , hashable\r\n    , split >=0.2\r\n    , text >=0.11\r\n    , transformers >=0.3 && <0.6\r\n    , unordered-containers >=0.2\r\n    , vector >=0.10\r\n    , vector-algorithms >=0.6\r\n  if impl(ghc <8.0)\r\n    build-depends:\r\n        semigroups >=0.10\r\n  default-language: Haskell2010\r\n\r\ntest-suite test\r\n  type: exitcode-stdio-1.0\r\n  main-is: main.hs\r\n  other-modules:\r\n      Spec\r\n      Paths_mono_traversable\r\n  hs-source-dirs:\r\n      test\r\n  ghc-options: -O0\r\n  build-depends:\r\n      HUnit\r\n    , QuickCheck\r\n    , base\r\n    , bytestring\r\n    , containers\r\n    , foldl\r\n    , hspec\r\n    , mono-traversable\r\n    , semigroups\r\n    , text\r\n    , transformers\r\n    , unordered-containers\r\n    , vector\r\n  default-language: Haskell2010\r\n\r\nbenchmark sorting\r\n  type: exitcode-stdio-1.0\r\n  main-is: sorting.hs\r\n  other-modules:\r\n      Paths_mono_traversable\r\n  hs-source-dirs:\r\n      bench\r\n  ghc-options: -Wall -O2\r\n  build-depends:\r\n      base\r\n    , gauge\r\n    , mono-traversable\r\n    , mwc-random\r\n    , vector\r\n  default-language: Haskell2010\r\n";
    }