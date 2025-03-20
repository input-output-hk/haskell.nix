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
      identifier = { name = "unordered-containers"; version = "0.2.20"; };
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
      url = "http://hackage.haskell.org/package/unordered-containers-0.2.20.tar.gz";
      sha256 = "d9cfb287cf00592d39dc9c3cac8b99627ea08f2c01798e70130fc39f7c90f11d";
    });
  }) // {
    package-description-override = "name:           unordered-containers\r\nversion:        0.2.20\r\nx-revision: 3\r\nsynopsis:       Efficient hashing-based container types\r\ndescription:\r\n  Efficient hashing-based container types.  The containers have been\r\n  optimized for performance critical use, both in terms of large data\r\n  quantities and high speed.\r\n  .\r\n  The declared cost of each operation is either worst-case or\r\n  amortized, but remains valid even if structures are shared.\r\n  .\r\n  /Security/\r\n  .\r\n  This package currently provides no defenses against hash collision attacks\r\n  such as HashDoS.\r\n  Users who need to store input from untrusted sources are advised to use\r\n  @Data.Map@ or @Data.Set@ from the @containers@ package instead.\r\nlicense:        BSD3\r\nlicense-file:   LICENSE\r\nauthor:         Johan Tibell\r\nmaintainer:     simon.jakobi@gmail.com, David.Feuer@gmail.com\r\nHomepage:       https://github.com/haskell-unordered-containers/unordered-containers\r\nbug-reports:    https://github.com/haskell-unordered-containers/unordered-containers/issues\r\ncopyright:      2010-2014 Johan Tibell\r\n                2010 Edward Z. Yang\r\ncategory:       Data\r\nbuild-type:     Simple\r\ncabal-version:  >=1.10\r\nextra-source-files: CHANGES.md\r\n\r\ntested-with:\r\n  GHC ==9.8.1\r\n   || ==9.6.3\r\n   || ==9.4.7\r\n   || ==9.2.8\r\n   || ==9.0.2\r\n   || ==8.10.7\r\n   || ==8.8.4\r\n   || ==8.6.5\r\n   || ==8.4.4\r\n   || ==8.2.2\r\n\r\nflag debug\r\n  description:  Enable debug support\r\n  default:      False\r\n\r\nlibrary\r\n  exposed-modules:\r\n    Data.HashMap.Internal\r\n    Data.HashMap.Internal.Array\r\n    Data.HashMap.Internal.Debug\r\n    Data.HashMap.Internal.List\r\n    Data.HashMap.Internal.Strict\r\n    Data.HashMap.Lazy\r\n    Data.HashMap.Strict\r\n    Data.HashSet\r\n    Data.HashSet.Internal\r\n\r\n  build-depends:\r\n    base >= 4.10 && < 5,\r\n    deepseq >= 1.4.3,\r\n    hashable >= 1.4 && < 1.6,\r\n    template-haskell < 2.23\r\n\r\n  default-language: Haskell2010\r\n\r\n  other-extensions:\r\n    RoleAnnotations,\r\n    UnboxedTuples,\r\n    ScopedTypeVariables,\r\n    MagicHash,\r\n    BangPatterns\r\n\r\n  ghc-options: -Wall -O2 -fwarn-tabs -ferror-spans\r\n\r\n  -- For dumping the generated code:\r\n  -- ghc-options: -ddump-simpl -ddump-stg-final -ddump-cmm -ddump-asm -ddump-to-file\r\n  -- ghc-options: -dsuppress-coercions -dsuppress-unfoldings -dsuppress-module-prefixes\r\n  -- ghc-options: -dsuppress-uniques -dsuppress-timestamps\r\n\r\n  if flag(debug)\r\n    cpp-options: -DASSERTS\r\n\r\ntest-suite unordered-containers-tests\r\n  hs-source-dirs: tests\r\n  main-is: Main.hs\r\n  type: exitcode-stdio-1.0\r\n  other-modules:\r\n    Regressions\r\n    Properties\r\n    Properties.HashMapLazy\r\n    Properties.HashMapStrict\r\n    Properties.HashSet\r\n    Properties.List\r\n    Strictness\r\n    Util.Key\r\n\r\n  build-depends:\r\n    base,\r\n    ChasingBottoms,\r\n    containers >= 0.5.8,\r\n    hashable,\r\n    HUnit,\r\n    QuickCheck >= 2.4.0.1,\r\n    random,\r\n    tasty >= 1.4.0.3,\r\n    tasty-hunit >= 0.10.0.3,\r\n    tasty-quickcheck >= 0.10.1.2,\r\n    unordered-containers\r\n\r\n  if impl(ghc >= 8.6)\r\n    build-depends:\r\n      nothunks >= 0.1.3\r\n\r\n  default-language: Haskell2010\r\n  ghc-options: -Wall -threaded -rtsopts -with-rtsopts=-N\r\n  cpp-options: -DASSERTS\r\n\r\nbenchmark benchmarks\r\n  hs-source-dirs: benchmarks\r\n  main-is: Benchmarks.hs\r\n  type: exitcode-stdio-1.0\r\n\r\n  other-modules:\r\n    Util.ByteString\r\n    Util.String\r\n    Util.Int\r\n\r\n  build-depends:\r\n    base >= 4.8.0,\r\n    bytestring >= 0.10.0.0,\r\n    containers,\r\n    deepseq,\r\n    hashable,\r\n    hashmap,\r\n    mtl,\r\n    random,\r\n    tasty-bench >= 0.3.1,\r\n    unordered-containers\r\n\r\n  default-language: Haskell2010\r\n  ghc-options: -Wall -O2 -rtsopts -with-rtsopts=-A32m\r\n  if impl(ghc >= 8.10)\r\n    ghc-options: \"-with-rtsopts=-A32m --nonmoving-gc\"\r\n  -- cpp-options: -DBENCH_containers_Map -DBENCH_containers_IntMap -DBENCH_hashmap_Map\r\n\r\nsource-repository head\r\n  type:     git\r\n  location: https://github.com/haskell-unordered-containers/unordered-containers.git\r\n";
  }