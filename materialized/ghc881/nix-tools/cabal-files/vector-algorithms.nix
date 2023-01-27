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
    flags = {
      boundschecks = true;
      unsafechecks = false;
      internalchecks = false;
      bench = true;
      properties = true;
      llvm = false;
      };
    package = {
      specVersion = "1.10";
      identifier = { name = "vector-algorithms"; version = "0.9.0.1"; };
      license = "BSD-3-Clause";
      copyright = "(c) 2008,2009,2010,2011,2012,2013,2014,2015 Dan Doel\n(c) 2015 Tim Baumann";
      maintainer = "Dan Doel <dan.doel@gmail.com>\nErik de Castro Lopo <erikd@mega-nerd.com>";
      author = "Dan Doel";
      homepage = "https://github.com/erikd/vector-algorithms/";
      url = "";
      synopsis = "Efficient algorithms for vector arrays";
      description = "Efficient algorithms for sorting vector arrays. At some stage\nother vector algorithms may be added.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bitvec" or (errorHandler.buildDepError "bitvec"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          ] ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "7.8")) (hsPkgs."tagged" or (errorHandler.buildDepError "tagged"));
        buildable = true;
        };
      tests = {
        "properties" = {
          depends = (pkgs.lib).optionals (!(!flags.properties)) [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."vector-algorithms" or (errorHandler.buildDepError "vector-algorithms"))
            ];
          buildable = if !flags.properties then false else true;
          };
        };
      benchmarks = {
        "simple-bench" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."mwc-random" or (errorHandler.buildDepError "mwc-random"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."vector-algorithms" or (errorHandler.buildDepError "vector-algorithms"))
            ];
          buildable = if !flags.bench then false else true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/vector-algorithms-0.9.0.1.tar.gz";
      sha256 = "2ba7c0d3a8f26ef3ada24ff4abe81609225ecbab3b5754f048f8a0a3ecc33841";
      });
    }) // {
    package-description-override = "name:              vector-algorithms\nversion:           0.9.0.1\nx-revision: 1\nlicense:           BSD3\nlicense-file:      LICENSE\nauthor:            Dan Doel\nmaintainer:        Dan Doel <dan.doel@gmail.com>\n                   Erik de Castro Lopo <erikd@mega-nerd.com>\ncopyright:         (c) 2008,2009,2010,2011,2012,2013,2014,2015 Dan Doel\n                   (c) 2015 Tim Baumann\nhomepage:          https://github.com/erikd/vector-algorithms/\ncategory:          Data\nsynopsis:          Efficient algorithms for vector arrays\ndescription:       Efficient algorithms for sorting vector arrays. At some stage\n                   other vector algorithms may be added.\nbuild-type:        Simple\ncabal-version:     >= 1.10\nextra-source-files: CHANGELOG.md\n\n\nflag BoundsChecks\n  description: Enable bounds checking\n  default: True\n\nflag UnsafeChecks\n  description: Enable bounds checking in unsafe operations at the cost of a\n               significant performance penalty.\n  default: False\n\nflag InternalChecks\n  description: Enable internal consistency checks at the cost of a\n               significant performance penalty.\n  default: False\n\nflag bench\n  description: Build a benchmarking program to test vector-algorithms\n               performance\n  default: True\n\nflag properties\n  description: Enable the quickcheck tests\n  default: True\n\n-- flag dump-simpl\n--   description: Dumps the simplified core during compilation\n--   default: False\n\nflag llvm\n  description: Build using llvm\n  default: False\n\nsource-repository head\n  type:     git\n  location: https://github.com/erikd/vector-algorithms/\n\nlibrary\n  hs-source-dirs: src\n  default-language: Haskell2010\n\n  build-depends: base >= 4.5 && < 5,\n                 bitvec >= 1.0 && < 1.2,\n                 vector >= 0.6 && < 0.14,\n                 primitive >=0.6.2.0 && <0.8,\n                 bytestring >= 0.9 && < 1.0\n\n  if ! impl (ghc >= 7.8)\n    build-depends: tagged >= 0.4 && < 0.9\n\n  exposed-modules:\n    Data.Vector.Algorithms\n    Data.Vector.Algorithms.Optimal\n    Data.Vector.Algorithms.Insertion\n    Data.Vector.Algorithms.Intro\n    Data.Vector.Algorithms.Merge\n    Data.Vector.Algorithms.Radix\n    Data.Vector.Algorithms.Search\n    Data.Vector.Algorithms.Heap\n    Data.Vector.Algorithms.AmericanFlag\n    Data.Vector.Algorithms.Tim\n\n  other-modules:\n    Data.Vector.Algorithms.Common\n\n  ghc-options:\n    -funbox-strict-fields\n\n  -- Cabal/Hackage complains about these\n  -- if flag(dump-simpl)\n  --   ghc-options: -ddump-simpl -ddump-to-file\n\n  if flag(llvm)\n    ghc-options: -fllvm\n\n  include-dirs:\n    include\n\n  install-includes:\n    vector.h\n\n  if flag(BoundsChecks)\n    cpp-options: -DVECTOR_BOUNDS_CHECKS\n\n  if flag(UnsafeChecks)\n    cpp-options: -DVECTOR_UNSAFE_CHECKS\n\n  if flag(InternalChecks)\n    cpp-options: -DVECTOR_INTERNAL_CHECKS\n\nbenchmark simple-bench\n  hs-source-dirs: bench/simple\n  type: exitcode-stdio-1.0\n  default-language: Haskell2010\n\n  if !flag(bench)\n    buildable: False\n\n  main-is: Main.hs\n\n  other-modules:\n    Blocks\n\n  build-depends: base, mwc-random, vector, vector-algorithms\n  ghc-options: -Wall\n\n  -- Cabal/Hackage complains about these\n  -- if flag(dump-simpl)\n  --   ghc-options: -ddump-simpl -ddump-to-file\n\n  if flag(llvm)\n    ghc-options: -fllvm\n\ntest-suite properties\n  hs-source-dirs: tests/properties\n  type: exitcode-stdio-1.0\n  main-is: Tests.hs\n  default-language: Haskell2010\n\n  other-modules:\n    Optimal\n    Properties\n    Util\n\n  if !flag(properties)\n    buildable: False\n  else\n    build-depends:\n      base,\n      bytestring,\n      containers,\n      QuickCheck > 2.9 && < 2.15,\n      vector,\n      vector-algorithms\n\n  if flag(llvm)\n    ghc-options: -fllvm\n\n";
    }