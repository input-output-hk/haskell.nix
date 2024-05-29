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
    flags = { debug = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "unordered-containers"; version = "0.2.19.1"; };
      license = "BSD-3-Clause";
      copyright = "2010-2014 Johan Tibell\n2010 Edward Z. Yang";
      maintainer = "simon.jakobi@gmail.com, David.Feuer@gmail.com";
      author = "Johan Tibell";
      homepage = "https://github.com/haskell-unordered-containers/unordered-containers";
      url = "";
      synopsis = "Efficient hashing-based container types";
      description = "Efficient hashing-based container types.  The containers have been\noptimized for performance critical use, both in terms of large data\nquantities and high speed.\n\nThe declared cost of each operation is either worst-case or\namortized, but remains valid even if structures are shared.\n\n/Security/\n\nThis package currently provides no defenses against hash collision attacks\nsuch as HashDoS.\nUsers who need to store input from untrusted sources are advised to use\n@Data.Map@ or @Data.Set@ from the @containers@ package instead.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
        ];
        buildable = true;
      };
      tests = {
        "unordered-containers-tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."ChasingBottoms" or (errorHandler.buildDepError "ChasingBottoms"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          ] ++ pkgs.lib.optional (compiler.isGhc && compiler.version.ge "8.6") (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"));
          buildable = true;
        };
      };
      benchmarks = {
        "benchmarks" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
            (hsPkgs."hashmap" or (errorHandler.buildDepError "hashmap"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."tasty-bench" or (errorHandler.buildDepError "tasty-bench"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/unordered-containers-0.2.19.1.tar.gz";
      sha256 = "1b27bec5e0d522b27a6029ebf4c4a6d40acbc083c787008e32fb55c4b1d128d2";
    });
  }) // {
    package-description-override = "name:           unordered-containers\nversion:        0.2.19.1\nx-revision:     3\nsynopsis:       Efficient hashing-based container types\ndescription:\n  Efficient hashing-based container types.  The containers have been\n  optimized for performance critical use, both in terms of large data\n  quantities and high speed.\n  .\n  The declared cost of each operation is either worst-case or\n  amortized, but remains valid even if structures are shared.\n  .\n  /Security/\n  .\n  This package currently provides no defenses against hash collision attacks\n  such as HashDoS.\n  Users who need to store input from untrusted sources are advised to use\n  @Data.Map@ or @Data.Set@ from the @containers@ package instead.\nlicense:        BSD3\nlicense-file:   LICENSE\nauthor:         Johan Tibell\nmaintainer:     simon.jakobi@gmail.com, David.Feuer@gmail.com\nHomepage:       https://github.com/haskell-unordered-containers/unordered-containers\nbug-reports:    https://github.com/haskell-unordered-containers/unordered-containers/issues\ncopyright:      2010-2014 Johan Tibell\n                2010 Edward Z. Yang\ncategory:       Data\nbuild-type:     Simple\ncabal-version:  >=1.10\nextra-source-files: CHANGES.md\n\ntested-with:\n  GHC ==9.8.1\n   || ==9.6.3\n   || ==9.4.7\n   || ==9.2.8\n   || ==9.0.2\n   || ==8.10.7\n   || ==8.8.4\n   || ==8.6.5\n   || ==8.4.4\n   || ==8.2.2\n\nflag debug\n  description:  Enable debug support\n  default:      False\n\nlibrary\n  exposed-modules:\n    Data.HashMap.Internal\n    Data.HashMap.Internal.Array\n    Data.HashMap.Internal.List\n    Data.HashMap.Internal.Strict\n    Data.HashMap.Lazy\n    Data.HashMap.Strict\n    Data.HashSet\n    Data.HashSet.Internal\n\n  build-depends:\n    base >= 4.10 && < 5,\n    deepseq >= 1.4.3,\n    hashable >= 1.2.5 && < 1.5,\n    template-haskell < 2.22\n\n  default-language: Haskell2010\n\n  other-extensions:\n    RoleAnnotations,\n    UnboxedTuples,\n    ScopedTypeVariables,\n    MagicHash,\n    BangPatterns\n\n  ghc-options: -Wall -O2 -fwarn-tabs -ferror-spans\n\n  -- For dumping the generated code:\n  -- ghc-options: -ddump-simpl -ddump-stg-final -ddump-cmm -ddump-asm -ddump-to-file\n  -- ghc-options: -dsuppress-coercions -dsuppress-unfoldings -dsuppress-module-prefixes\n  -- ghc-options: -dsuppress-uniques -dsuppress-timestamps\n\n  if flag(debug)\n    cpp-options: -DASSERTS\n\ntest-suite unordered-containers-tests\n  hs-source-dirs: tests\n  main-is: Main.hs\n  type: exitcode-stdio-1.0\n  other-modules:\n    Regressions\n    Properties\n    Properties.HashMapLazy\n    Properties.HashMapStrict\n    Properties.HashSet\n    Properties.List\n    Strictness\n\n  build-depends:\n    base,\n    ChasingBottoms,\n    containers >= 0.5.8,\n    hashable,\n    HUnit,\n    QuickCheck >= 2.4.0.1,\n    random,\n    tasty >= 1.4.0.3,\n    tasty-hunit >= 0.10.0.3,\n    tasty-quickcheck >= 0.10.1.2,\n    unordered-containers\n\n  if impl(ghc >= 8.6)\n    build-depends:\n      nothunks >= 0.1.3\n\n  default-language: Haskell2010\n  ghc-options: -Wall\n  cpp-options: -DASSERTS\n\nbenchmark benchmarks\n  hs-source-dirs: benchmarks\n  main-is: Benchmarks.hs\n  type: exitcode-stdio-1.0\n\n  other-modules:\n    Util.ByteString\n    Util.String\n    Util.Int\n\n  build-depends:\n    base >= 4.8.0,\n    bytestring >= 0.10.0.0,\n    containers,\n    deepseq,\n    hashable,\n    hashmap,\n    mtl,\n    random,\n    tasty-bench >= 0.3.1,\n    unordered-containers\n\n  default-language: Haskell2010\n  ghc-options: -Wall -O2 -rtsopts -with-rtsopts=-A32m\n  if impl(ghc >= 8.10)\n    ghc-options: \"-with-rtsopts=-A32m --nonmoving-gc\"\n  -- cpp-options: -DBENCH_containers_Map -DBENCH_containers_IntMap -DBENCH_hashmap_Map\n\nsource-repository head\n  type:     git\n  location: https://github.com/haskell-unordered-containers/unordered-containers.git\n";
  }