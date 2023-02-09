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
      maintainer = "Bryan O'Sullivan <bos@serpentine.com>, Ben Gamari <ben@smart-cactus.org>";
      author = "Bryan O'Sullivan <bos@serpentine.com>";
      homepage = "https://github.com/bgamari/attoparsec";
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
    package-description-override = "name:            attoparsec\r\nversion:         0.14.4\r\nx-revision: 2\r\nlicense:         BSD3\r\nlicense-file:    LICENSE\r\ncategory:        Text, Parsing\r\nauthor:          Bryan O'Sullivan <bos@serpentine.com>\r\nmaintainer:      Bryan O'Sullivan <bos@serpentine.com>, Ben Gamari <ben@smart-cactus.org>\r\nstability:       experimental\r\ntested-with:     GHC == 7.4.2, GHC ==7.6.3, GHC ==7.8.4, GHC ==7.10.3, GHC ==8.0.2, GHC ==8.2.2, GHC==8.4.4, GHC==8.6.5, GHC==8.8.4, GHC==8.10.7, GHC ==9.0.2, GHC ==9.2.1\r\nsynopsis:        Fast combinator parsing for bytestrings and text\r\ncabal-version:   2.0\r\nhomepage:        https://github.com/bgamari/attoparsec\r\nbug-reports:     https://github.com/bgamari/attoparsec/issues\r\nbuild-type:      Simple\r\ndescription:\r\n    A fast parser combinator library, aimed particularly at dealing\r\n    efficiently with network protocols and complicated text/binary\r\n    file formats.\r\nextra-source-files:\r\n    README.markdown\r\n    benchmarks/*.txt\r\n    benchmarks/json-data/*.json\r\n    benchmarks/Makefile\r\n    benchmarks/med.txt.bz2\r\n    changelog.md\r\n    examples/*.c\r\n    examples/*.hs\r\n    examples/Makefile\r\n\r\nFlag developer\r\n  Description: Whether to build the library in development mode\r\n  Default: False\r\n  Manual: True\r\n\r\n-- We need to test and benchmark these modules,\r\n-- but do not want to expose them to end users\r\nlibrary attoparsec-internal\r\n  hs-source-dirs: internal\r\n  build-depends: array,\r\n                 base >= 4.5 && < 5,\r\n                 bytestring <0.12,\r\n                 text >= 1.1.1.3\r\n  if !impl(ghc >= 8.0)\r\n    build-depends: semigroups >=0.16.1 && <0.21\r\n  exposed-modules: Data.Attoparsec.ByteString.Buffer\r\n                   Data.Attoparsec.ByteString.FastSet\r\n                   Data.Attoparsec.Internal.Compat\r\n                   Data.Attoparsec.Internal.Fhthagn\r\n                   Data.Attoparsec.Text.Buffer\r\n                   Data.Attoparsec.Text.FastSet\r\n  ghc-options: -O2 -Wall\r\n  default-language: Haskell2010\r\n\r\nlibrary\r\n  build-depends: array,\r\n                 base >= 4.5 && < 5,\r\n                 bytestring <0.12,\r\n                 containers,\r\n                 deepseq,\r\n                 scientific >= 0.3.1 && < 0.4,\r\n                 transformers >= 0.2 && (< 0.4 || >= 0.4.1.0) && < 0.7,\r\n                 text >= 1.1.1.3,\r\n                 ghc-prim <0.10,\r\n                 attoparsec-internal\r\n  if impl(ghc < 7.4)\r\n    build-depends:\r\n      bytestring < 0.10.4.0\r\n\r\n  if !impl(ghc >= 8.0)\r\n    -- Data.Semigroup && Control.Monad.Fail are available in base-4.9+\r\n    build-depends: fail == 4.9.*,\r\n                   semigroups >=0.16.1 && <0.21\r\n\r\n  exposed-modules: Data.Attoparsec\r\n                   Data.Attoparsec.ByteString\r\n                   Data.Attoparsec.ByteString.Char8\r\n                   Data.Attoparsec.ByteString.Lazy\r\n                   Data.Attoparsec.Char8\r\n                   Data.Attoparsec.Combinator\r\n                   Data.Attoparsec.Internal\r\n                   Data.Attoparsec.Internal.Types\r\n                   Data.Attoparsec.Lazy\r\n                   Data.Attoparsec.Number\r\n                   Data.Attoparsec.Text\r\n                   Data.Attoparsec.Text.Lazy\r\n                   Data.Attoparsec.Types\r\n                   Data.Attoparsec.Zepto\r\n  other-modules:   Data.Attoparsec.ByteString.Internal\r\n                   Data.Attoparsec.Text.Internal\r\n  ghc-options: -O2 -Wall\r\n\r\n  default-language: Haskell2010\r\n\r\n  if flag(developer)\r\n    ghc-prof-options: -auto-all\r\n    ghc-options: -Werror\r\n\r\ntest-suite attoparsec-tests\r\n  type:           exitcode-stdio-1.0\r\n  hs-source-dirs: tests\r\n  main-is:        QC.hs\r\n  other-modules:  QC.Buffer\r\n                  QC.ByteString\r\n                  QC.Combinator\r\n                  QC.Common\r\n                  QC.IPv6.Internal\r\n                  QC.IPv6.Types\r\n                  QC.Rechunked\r\n                  QC.Simple\r\n                  QC.Text\r\n                  QC.Text.FastSet\r\n                  QC.Text.Regressions\r\n\r\n  ghc-options:\r\n    -Wall -threaded -rtsopts\r\n\r\n  if flag(developer)\r\n    ghc-options: -Werror\r\n\r\n  build-depends:\r\n    array,\r\n    attoparsec,\r\n    attoparsec-internal,\r\n    base,\r\n    bytestring,\r\n    deepseq >= 1.1,\r\n    QuickCheck >= 2.13.2 && < 2.15,\r\n    quickcheck-unicode,\r\n    scientific,\r\n    tasty >= 0.11,\r\n    tasty-quickcheck >= 0.8,\r\n    text,\r\n    transformers,\r\n    vector\r\n\r\n  default-language: Haskell2010\r\n\r\n  if !impl(ghc >= 8.0)\r\n    -- Data.Semigroup && Control.Monad.Fail are available in base-4.9+\r\n    build-depends: fail == 4.9.*,\r\n                   semigroups >=0.16.1 && <0.19\r\n\r\nbenchmark attoparsec-benchmarks\r\n  type: exitcode-stdio-1.0\r\n  hs-source-dirs: benchmarks benchmarks/warp-3.0.1.1\r\n  ghc-options: -O2 -Wall -rtsopts\r\n  main-is: Benchmarks.hs\r\n  other-modules:\r\n    Aeson\r\n    Common\r\n    Genome\r\n    HeadersByteString\r\n    HeadersByteString.Atto\r\n    HeadersText\r\n    Links\r\n    Network.Wai.Handler.Warp.ReadInt\r\n    Network.Wai.Handler.Warp.RequestHeader\r\n    Numbers\r\n    Sets\r\n    TextFastSet\r\n    Warp\r\n  ghc-options: -O2 -Wall\r\n\r\n  if flag(developer)\r\n    ghc-options: -Werror\r\n\r\n  build-depends:\r\n    array,\r\n    attoparsec,\r\n    attoparsec-internal,\r\n    base == 4.*,\r\n    bytestring >= 0.10.4.0,\r\n    case-insensitive,\r\n    containers,\r\n    deepseq >= 1.1,\r\n    directory,\r\n    filepath,\r\n    ghc-prim,\r\n    http-types,\r\n    parsec >= 3.1.2,\r\n    scientific,\r\n    tasty-bench >= 0.3,\r\n    text >= 1.1.1.0,\r\n    transformers,\r\n    unordered-containers,\r\n    vector\r\n\r\n  default-language: Haskell2010\r\n\r\n  if !impl(ghc >= 8.0)\r\n    -- Data.Semigroup && Control.Monad.Fail are available in base-4.9+\r\n    build-depends: fail == 4.9.*,\r\n                   semigroups >=0.16.1 && <0.19\r\n\r\nsource-repository head\r\n  type:     git\r\n  location: https://github.com/bgamari/attoparsec\r\n";
    }