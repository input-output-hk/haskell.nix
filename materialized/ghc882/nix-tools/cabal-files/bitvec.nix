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
    flags = { libgmp = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "bitvec"; version = "1.1.3.0"; };
      license = "BSD-3-Clause";
      copyright = "2019-2022 Andrew Lelechenko, 2012-2016 James Cook";
      maintainer = "Andrew Lelechenko <andrew.lelechenko@gmail.com>";
      author = "Andrew Lelechenko <andrew.lelechenko@gmail.com>,\nJames Cook <mokus@deepbondi.net>";
      homepage = "https://github.com/Bodigrim/bitvec";
      url = "";
      synopsis = "Space-efficient bit vectors";
      description = "A newtype over 'Bool' with a better 'Vector' instance: 8x less memory, up to 1000x faster.\n\nThe <https://hackage.haskell.org/package/vector vector>\npackage represents unboxed arrays of 'Bool's\nspending 1 byte (8 bits) per boolean.\nThis library provides a newtype wrapper 'Bit' and a custom instance\nof an unboxed 'Vector', which packs bits densely,\nachieving an __8x smaller memory footprint.__\nThe performance stays mostly the same;\nthe most significant degradation happens for random writes\n(up to 10% slower).\nOn the other hand, for certain bulk bit operations\n'Vector' 'Bit' is up to 1000x faster than 'Vector' 'Bool'.\n\n=== Thread safety\n\n* \"Data.Bit\" is faster, but writes and flips are thread-unsafe.\nThis is because naive updates are not atomic:\nthey read the whole word from memory,\nthen modify a bit, then write the whole word back.\n* \"Data.Bit.ThreadSafe\" is slower (up to 20%),\nbut writes and flips are thread-safe.\n\n=== Similar packages\n\n* <https://hackage.haskell.org/package/bv bv> and\n<https://hackage.haskell.org/package/bv-little bv-little>\ndo not offer mutable vectors.\n\n* <https://hackage.haskell.org/package/array array>\nis memory-efficient for 'Bool', but lacks\na handy 'Vector' interface and is not thread-safe.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          ] ++ (if compiler.isGhc && (compiler.version).lt "9.0"
          then [
            (hsPkgs."integer-gmp" or (errorHandler.buildDepError "integer-gmp"))
            ]
          else [
            (hsPkgs."ghc-bignum" or (errorHandler.buildDepError "ghc-bignum"))
            ]);
        libs = (pkgs.lib).optional (flags.libgmp) (pkgs."gmp" or (errorHandler.sysDepError "gmp"));
        buildable = true;
        };
      tests = {
        "bitvec-tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bitvec" or (errorHandler.buildDepError "bitvec"))
            (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
            (hsPkgs."quickcheck-classes-base" or (errorHandler.buildDepError "quickcheck-classes-base"))
            (hsPkgs."quickcheck-classes" or (errorHandler.buildDepError "quickcheck-classes"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            ] ++ (if compiler.isGhc && (compiler.version).lt "9.0"
            then [
              (hsPkgs."integer-gmp" or (errorHandler.buildDepError "integer-gmp"))
              ]
            else [
              (hsPkgs."ghc-bignum" or (errorHandler.buildDepError "ghc-bignum"))
              ]);
          buildable = true;
          };
        };
      benchmarks = {
        "bitvec-bench" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bitvec" or (errorHandler.buildDepError "bitvec"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."tasty-bench" or (errorHandler.buildDepError "tasty-bench"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            ] ++ (if compiler.isGhc && (compiler.version).lt "9.0"
            then [
              (hsPkgs."integer-gmp" or (errorHandler.buildDepError "integer-gmp"))
              ]
            else [
              (hsPkgs."ghc-bignum" or (errorHandler.buildDepError "ghc-bignum"))
              ]);
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/bitvec-1.1.3.0.tar.gz";
      sha256 = "1c4c3af50a1fcf46e46e2fa9e0493fffaf62de3607ae7553d18015f8a1a60551";
      });
    }) // {
    package-description-override = "name: bitvec\nversion: 1.1.3.0\ncabal-version: >=1.10\nbuild-type: Simple\nlicense: BSD3\nlicense-file: LICENSE\ncopyright: 2019-2022 Andrew Lelechenko, 2012-2016 James Cook\nmaintainer: Andrew Lelechenko <andrew.lelechenko@gmail.com>\nhomepage: https://github.com/Bodigrim/bitvec\nsynopsis: Space-efficient bit vectors\ndescription:\n  A newtype over 'Bool' with a better 'Vector' instance: 8x less memory, up to 1000x faster.\n  .\n  The <https://hackage.haskell.org/package/vector vector>\n  package represents unboxed arrays of 'Bool's\n  spending 1 byte (8 bits) per boolean.\n  This library provides a newtype wrapper 'Bit' and a custom instance\n  of an unboxed 'Vector', which packs bits densely,\n  achieving an __8x smaller memory footprint.__\n  The performance stays mostly the same;\n  the most significant degradation happens for random writes\n  (up to 10% slower).\n  On the other hand, for certain bulk bit operations\n  'Vector' 'Bit' is up to 1000x faster than 'Vector' 'Bool'.\n  .\n  === Thread safety\n  .\n  * \"Data.Bit\" is faster, but writes and flips are thread-unsafe.\n    This is because naive updates are not atomic:\n    they read the whole word from memory,\n    then modify a bit, then write the whole word back.\n  * \"Data.Bit.ThreadSafe\" is slower (up to 20%),\n    but writes and flips are thread-safe.\n  .\n  === Similar packages\n  .\n  * <https://hackage.haskell.org/package/bv bv> and\n    <https://hackage.haskell.org/package/bv-little bv-little>\n    do not offer mutable vectors.\n  .\n  * <https://hackage.haskell.org/package/array array>\n    is memory-efficient for 'Bool', but lacks\n    a handy 'Vector' interface and is not thread-safe.\n\ncategory: Data, Bit Vectors\nauthor: Andrew Lelechenko <andrew.lelechenko@gmail.com>,\n        James Cook <mokus@deepbondi.net>\n\ntested-with: GHC ==8.0.2 GHC ==8.2.2 GHC ==8.4.4 GHC ==8.6.5 GHC ==8.8.1 GHC ==8.8.2 GHC ==8.8.4 GHC ==8.10.7 GHC ==9.0.1 GHC ==9.2.1\nextra-source-files:\n  changelog.md\n  README.md\n\nsource-repository head\n  type: git\n  location: git://github.com/Bodigrim/bitvec.git\n\nflag libgmp\n  description:\n    Link against the GMP library for the ultimate performance of\n    `zipBits`, `invertBits` and `countBits`. Users are strongly encouraged\n    to enable this flag whenever possible.\n  default: False\n  manual: True\n\nlibrary\n  exposed-modules:\n    Data.Bit\n    Data.Bit.ThreadSafe\n  build-depends:\n    base >=4.9 && <5,\n    bytestring >=0.10,\n    deepseq,\n    primitive >=0.5,\n    vector >=0.11 && <0.14\n  default-language: Haskell2010\n  hs-source-dirs: src\n  other-modules:\n    Data.Bit.F2Poly\n    Data.Bit.F2PolyTS\n    Data.Bit.Gmp\n    Data.Bit.Immutable\n    Data.Bit.ImmutableTS\n    Data.Bit.Internal\n    Data.Bit.InternalTS\n    Data.Bit.Mutable\n    Data.Bit.MutableTS\n    Data.Bit.PdepPext\n    Data.Bit.Utils\n  ghc-options: -O2 -Wall -Wcompat\n  include-dirs: src\n\n  if impl(ghc <9.0)\n    build-depends: integer-gmp\n  else\n    build-depends: ghc-bignum\n\n  if flag(libgmp)\n    extra-libraries: gmp\n    cpp-options: -DUseLibGmp\n\ntest-suite bitvec-tests\n  type: exitcode-stdio-1.0\n  main-is: Main.hs\n  build-depends:\n    base,\n    bitvec,\n    primitive >=0.5,\n    quickcheck-classes-base,\n    quickcheck-classes >=0.6.1,\n    vector >=0.11,\n    tasty,\n    tasty-quickcheck\n  default-language: Haskell2010\n  hs-source-dirs: test\n  other-modules:\n    Support\n    Tests.Conc\n    Tests.F2Poly\n    Tests.MVector\n    Tests.MVectorTS\n    Tests.SetOps\n    Tests.SetOpsTS\n    Tests.Vector\n  ghc-options: -Wall -threaded -rtsopts -Wcompat\n  include-dirs: test\n\n  if impl(ghc <9.0)\n    build-depends: integer-gmp\n  else\n    build-depends: ghc-bignum\n\nbenchmark bitvec-bench\n  build-depends:\n    base,\n    bitvec,\n    containers,\n    random,\n    tasty-bench,\n    vector\n  type: exitcode-stdio-1.0\n  main-is: Bench.hs\n  default-language: Haskell2010\n  hs-source-dirs: bench\n  other-modules:\n    Bench.BitIndex\n    Bench.GCD\n    Bench.Invert\n    Bench.Intersection\n    Bench.Product\n    Bench.RandomFlip\n    Bench.RandomRead\n    Bench.RandomWrite\n    Bench.Remainder\n    Bench.Reverse\n    Bench.Sum\n    Bench.Union\n  ghc-options: -O2 -Wall -Wcompat\n\n  if impl(ghc <9.0)\n    build-depends: integer-gmp\n  else\n    build-depends: ghc-bignum\n";
    }