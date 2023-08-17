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
    flags = { developer = false; };
    package = {
      specVersion = "2.0";
      identifier = { name = "attoparsec"; version = "0.14.4"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Ben Gamari <ben@smart-cactus.org>";
      author = "Bryan O'Sullivan <bos@serpentine.com>";
      homepage = "https://github.com/haskell/attoparsec";
      url = "";
      synopsis = "Fast combinator parsing for bytestrings and text";
      description = "A fast parser combinator library, aimed particularly at dealing\nefficiently with network protocols and complicated text/binary\nfile formats.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = ([
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
          (hsPkgs."attoparsec".components.sublibs.attoparsec-internal or (errorHandler.buildDepError "attoparsec:attoparsec-internal"))
          ] ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).lt "7.4") (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))) ++ (pkgs.lib).optionals (!(compiler.isGhc && (compiler.version).ge "8.0")) [
          (hsPkgs."fail" or (errorHandler.buildDepError "fail"))
          (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
          ];
        buildable = true;
        };
      sublibs = {
        "attoparsec-internal" = {
          depends = [
            (hsPkgs."array" or (errorHandler.buildDepError "array"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            ] ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8.0")) (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"));
          buildable = true;
          };
        };
      tests = {
        "attoparsec-tests" = {
          depends = [
            (hsPkgs."array" or (errorHandler.buildDepError "array"))
            (hsPkgs."attoparsec" or (errorHandler.buildDepError "attoparsec"))
            (hsPkgs."attoparsec".components.sublibs.attoparsec-internal or (errorHandler.buildDepError "attoparsec:attoparsec-internal"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."quickcheck-unicode" or (errorHandler.buildDepError "quickcheck-unicode"))
            (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            ] ++ (pkgs.lib).optionals (!(compiler.isGhc && (compiler.version).ge "8.0")) [
            (hsPkgs."fail" or (errorHandler.buildDepError "fail"))
            (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
            ];
          buildable = true;
          };
        };
      benchmarks = {
        "attoparsec-benchmarks" = {
          depends = [
            (hsPkgs."array" or (errorHandler.buildDepError "array"))
            (hsPkgs."attoparsec" or (errorHandler.buildDepError "attoparsec"))
            (hsPkgs."attoparsec".components.sublibs.attoparsec-internal or (errorHandler.buildDepError "attoparsec:attoparsec-internal"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."case-insensitive" or (errorHandler.buildDepError "case-insensitive"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
            (hsPkgs."http-types" or (errorHandler.buildDepError "http-types"))
            (hsPkgs."parsec" or (errorHandler.buildDepError "parsec"))
            (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
            (hsPkgs."tasty-bench" or (errorHandler.buildDepError "tasty-bench"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            ] ++ (pkgs.lib).optionals (!(compiler.isGhc && (compiler.version).ge "8.0")) [
            (hsPkgs."fail" or (errorHandler.buildDepError "fail"))
            (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/attoparsec-0.14.4.tar.gz";
      sha256 = "3f337fe58624565de12426f607c23e60c7b09c86b4e3adfc827ca188c9979e6c";
      });
    }) // {
    package-description-override = "cabal-version:   2.0\n  -- 2.0 needed for internal libraries\nname:            attoparsec\nversion:         0.14.4\nx-revision:      3\nlicense:         BSD3\nlicense-file:    LICENSE\ncategory:        Text, Parsing\nauthor:          Bryan O'Sullivan <bos@serpentine.com>\nmaintainer:      Ben Gamari <ben@smart-cactus.org>\nstability:       experimental\nsynopsis:        Fast combinator parsing for bytestrings and text\nhomepage:        https://github.com/haskell/attoparsec\nbug-reports:     https://github.com/haskell/attoparsec/issues\nbuild-type:      Simple\ndescription:\n    A fast parser combinator library, aimed particularly at dealing\n    efficiently with network protocols and complicated text/binary\n    file formats.\n\ntested-with:\n  GHC == 9.6.0\n  GHC == 9.4.4\n  GHC == 9.2.6\n  GHC == 9.0.2\n  GHC == 8.10.7\n  GHC == 8.8.4\n  GHC == 8.6.5\n  GHC == 8.4.4\n  GHC == 8.2.2\n  GHC == 8.0.2\n  GHC == 7.10.3\n  GHC == 7.8.4\n  GHC == 7.6.3\n  GHC == 7.4.2\n\nextra-source-files:\n    README.markdown\n    benchmarks/*.txt\n    benchmarks/json-data/*.json\n    benchmarks/Makefile\n    benchmarks/med.txt.bz2\n    changelog.md\n    examples/*.c\n    examples/*.hs\n    examples/Makefile\n\nFlag developer\n  Description: Whether to build the library in development mode\n  Default: False\n  Manual: True\n\n-- We need to test and benchmark these modules,\n-- but do not want to expose them to end users\nlibrary attoparsec-internal\n  hs-source-dirs: internal\n  build-depends: array,\n                 base >= 4.5 && < 5,\n                 bytestring <0.12,\n                 text >= 1.1.1.3\n  if !impl(ghc >= 8.0)\n    build-depends: semigroups >=0.16.1 && <0.21\n  exposed-modules: Data.Attoparsec.ByteString.Buffer\n                   Data.Attoparsec.ByteString.FastSet\n                   Data.Attoparsec.Internal.Compat\n                   Data.Attoparsec.Internal.Fhthagn\n                   Data.Attoparsec.Text.Buffer\n                   Data.Attoparsec.Text.FastSet\n  ghc-options: -O2 -Wall\n  default-language: Haskell2010\n\nlibrary\n  build-depends: array,\n                 base >= 4.5 && < 5,\n                 bytestring <0.12,\n                 containers,\n                 deepseq,\n                 scientific >= 0.3.1 && < 0.4,\n                 transformers >= 0.2 && (< 0.4 || >= 0.4.1.0) && < 0.7,\n                 text >= 1.1.1.3,\n                 ghc-prim < 0.11,\n                 attoparsec-internal\n  if impl(ghc < 7.4)\n    build-depends:\n      bytestring < 0.10.4.0\n\n  if !impl(ghc >= 8.0)\n    -- Data.Semigroup && Control.Monad.Fail are available in base-4.9+\n    build-depends: fail == 4.9.*,\n                   semigroups >=0.16.1 && <0.21\n\n  exposed-modules: Data.Attoparsec\n                   Data.Attoparsec.ByteString\n                   Data.Attoparsec.ByteString.Char8\n                   Data.Attoparsec.ByteString.Lazy\n                   Data.Attoparsec.Char8\n                   Data.Attoparsec.Combinator\n                   Data.Attoparsec.Internal\n                   Data.Attoparsec.Internal.Types\n                   Data.Attoparsec.Lazy\n                   Data.Attoparsec.Number\n                   Data.Attoparsec.Text\n                   Data.Attoparsec.Text.Lazy\n                   Data.Attoparsec.Types\n                   Data.Attoparsec.Zepto\n  other-modules:   Data.Attoparsec.ByteString.Internal\n                   Data.Attoparsec.Text.Internal\n  ghc-options: -O2 -Wall\n\n  default-language: Haskell2010\n\n  if flag(developer)\n    ghc-prof-options: -auto-all\n    ghc-options: -Werror\n\ntest-suite attoparsec-tests\n  type:           exitcode-stdio-1.0\n  hs-source-dirs: tests\n  main-is:        QC.hs\n  other-modules:  QC.Buffer\n                  QC.ByteString\n                  QC.Combinator\n                  QC.Common\n                  QC.IPv6.Internal\n                  QC.IPv6.Types\n                  QC.Rechunked\n                  QC.Simple\n                  QC.Text\n                  QC.Text.FastSet\n                  QC.Text.Regressions\n\n  ghc-options:\n    -Wall -threaded -rtsopts\n\n  if flag(developer)\n    ghc-options: -Werror\n\n  build-depends:\n    array,\n    attoparsec,\n    attoparsec-internal,\n    base,\n    bytestring,\n    deepseq >= 1.1,\n    QuickCheck >= 2.13.2 && < 2.15,\n    quickcheck-unicode,\n    scientific,\n    tasty >= 0.11,\n    tasty-quickcheck >= 0.8,\n    text,\n    transformers,\n    vector\n\n  default-language: Haskell2010\n\n  if !impl(ghc >= 8.0)\n    -- Data.Semigroup && Control.Monad.Fail are available in base-4.9+\n    build-depends: fail == 4.9.*,\n                   semigroups >=0.16.1 && <0.19\n\nbenchmark attoparsec-benchmarks\n  type: exitcode-stdio-1.0\n  hs-source-dirs: benchmarks benchmarks/warp-3.0.1.1\n  ghc-options: -O2 -Wall -rtsopts\n  main-is: Benchmarks.hs\n  other-modules:\n    Aeson\n    Common\n    Genome\n    HeadersByteString\n    HeadersByteString.Atto\n    HeadersText\n    Links\n    Network.Wai.Handler.Warp.ReadInt\n    Network.Wai.Handler.Warp.RequestHeader\n    Numbers\n    Sets\n    TextFastSet\n    Warp\n  ghc-options: -O2 -Wall\n\n  if flag(developer)\n    ghc-options: -Werror\n\n  build-depends:\n    array,\n    attoparsec,\n    attoparsec-internal,\n    base == 4.*,\n    bytestring >= 0.10.4.0,\n    case-insensitive,\n    containers,\n    deepseq >= 1.1,\n    directory,\n    filepath,\n    ghc-prim,\n    http-types,\n    parsec >= 3.1.2,\n    scientific,\n    tasty-bench >= 0.3,\n    text >= 1.1.1.0,\n    transformers,\n    unordered-containers,\n    vector\n\n  default-language: Haskell2010\n\n  if !impl(ghc >= 8.0)\n    -- Data.Semigroup && Control.Monad.Fail are available in base-4.9+\n    build-depends: fail == 4.9.*,\n                   semigroups >=0.16.1 && <0.19\n\nsource-repository head\n  type:     git\n  location: https://github.com/haskell/attoparsec.git\n";
    }