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
    flags = { newtime15 = true; };
    package = {
      specVersion = "1.10";
      identifier = { name = "serialise"; version = "0.2.6.0"; };
      license = "BSD-3-Clause";
      copyright = "2015-2017 Duncan Coutts,\n2015-2017 Well-Typed LLP,\n2015 IRIS Connect Ltd";
      maintainer = "duncan@community.haskell.org, ben@smart-cactus.org";
      author = "Duncan Coutts";
      homepage = "https://github.com/well-typed/cborg";
      url = "";
      synopsis = "A binary serialisation library for Haskell values.";
      description = "This package (formerly @binary-serialise-cbor@) provides pure, efficient\nserialization of Haskell values directly into @ByteString@s for storage or\ntransmission purposes. By providing a set of type class instances, you can\nalso serialise any custom data type you have as well.\n\nThe underlying binary format used is the 'Concise Binary Object\nRepresentation', or CBOR, specified in RFC 7049. As a result,\nserialised Haskell values have implicit structure outside of the\nHaskell program itself, meaning they can be inspected or analyzed\nwithout custom tools.\n\nAn implementation of the standard bijection between CBOR and JSON is provided\nby the [cborg-json](/package/cborg-json) package. Also see\n[cbor-tool](/package/cbor-tool) for a convenient command-line utility for\nworking with CBOR data.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
          (hsPkgs."half" or (errorHandler.buildDepError "half"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
          (hsPkgs."strict" or (errorHandler.buildDepError "strict"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."these" or (errorHandler.buildDepError "these"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          ] ++ (if flags.newtime15
          then [ (hsPkgs."time" or (errorHandler.buildDepError "time")) ]
          else [
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."old-locale" or (errorHandler.buildDepError "old-locale"))
            ]);
        buildable = true;
        };
      tests = {
        "tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
            (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
            (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."quickcheck-instances" or (errorHandler.buildDepError "quickcheck-instances"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            ];
          buildable = true;
          };
        };
      benchmarks = {
        "instances" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
            (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            ] ++ (if flags.newtime15
            then [ (hsPkgs."time" or (errorHandler.buildDepError "time")) ]
            else [
              (hsPkgs."time" or (errorHandler.buildDepError "time"))
              (hsPkgs."old-locale" or (errorHandler.buildDepError "old-locale"))
              ]);
          buildable = true;
          };
        "micro" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
            (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."cereal" or (errorHandler.buildDepError "cereal"))
            (hsPkgs."cereal-vector" or (errorHandler.buildDepError "cereal-vector"))
            (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
            (hsPkgs."store" or (errorHandler.buildDepError "store"))
            ];
          buildable = true;
          };
        "versus" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."array" or (errorHandler.buildDepError "array"))
            (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
            (hsPkgs."fail" or (errorHandler.buildDepError "fail"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
            (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."cereal" or (errorHandler.buildDepError "cereal"))
            (hsPkgs."half" or (errorHandler.buildDepError "half"))
            (hsPkgs."tar" or (errorHandler.buildDepError "tar"))
            (hsPkgs."zlib" or (errorHandler.buildDepError "zlib"))
            (hsPkgs."pretty" or (errorHandler.buildDepError "pretty"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."store" or (errorHandler.buildDepError "store"))
            (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
            ] ++ (if flags.newtime15
            then [ (hsPkgs."time" or (errorHandler.buildDepError "time")) ]
            else [
              (hsPkgs."time" or (errorHandler.buildDepError "time"))
              (hsPkgs."old-locale" or (errorHandler.buildDepError "old-locale"))
              ]);
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/serialise-0.2.6.0.tar.gz";
      sha256 = "93ff1888e1972999f14663072b38efcfd0c1481b4ec8e30ddc9c5ce97681a516";
      });
    }) // {
    package-description-override = "name:                serialise\r\nversion:             0.2.6.0\r\nx-revision: 1\r\nsynopsis:            A binary serialisation library for Haskell values.\r\ndescription:\r\n  This package (formerly @binary-serialise-cbor@) provides pure, efficient\r\n  serialization of Haskell values directly into @ByteString@s for storage or\r\n  transmission purposes. By providing a set of type class instances, you can\r\n  also serialise any custom data type you have as well.\r\n  .\r\n  The underlying binary format used is the 'Concise Binary Object\r\n  Representation', or CBOR, specified in RFC 7049. As a result,\r\n  serialised Haskell values have implicit structure outside of the\r\n  Haskell program itself, meaning they can be inspected or analyzed\r\n  without custom tools.\r\n  .\r\n  An implementation of the standard bijection between CBOR and JSON is provided\r\n  by the [cborg-json](/package/cborg-json) package. Also see\r\n  [cbor-tool](/package/cbor-tool) for a convenient command-line utility for\r\n  working with CBOR data.\r\nhomepage:            https://github.com/well-typed/cborg\r\nlicense:             BSD3\r\nlicense-file:        LICENSE.txt\r\nauthor:              Duncan Coutts\r\nmaintainer:          duncan@community.haskell.org, ben@smart-cactus.org\r\nbug-reports:         https://github.com/well-typed/cborg/issues\r\ncopyright:           2015-2017 Duncan Coutts,\r\n                     2015-2017 Well-Typed LLP,\r\n                     2015 IRIS Connect Ltd\r\ncabal-version:       >=1.10\r\ncategory:            Codec\r\nbuild-type:          Simple\r\ntested-with:\r\n  GHC == 8.4.4,\r\n  GHC == 8.6.5,\r\n  GHC == 8.8.3,\r\n  GHC == 8.10.7,\r\n  GHC == 9.0.1,\r\n  GHC == 9.2.2,\r\n  GHC == 9.4.2\r\n\r\nextra-source-files:\r\n  ChangeLog.md\r\n\r\nsource-repository head\r\n  type: git\r\n  location: https://github.com/well-typed/cborg.git\r\n\r\n--------------------------------------------------------------------------------\r\n-- Flags\r\n\r\nflag newtime15\r\n  default: True\r\n  manual: False\r\n  description: Use the new time 1.5 library\r\n\r\nlibrary\r\n  default-language:  Haskell2010\r\n  ghc-options:       -Wall\r\n  hs-source-dirs:    src\r\n\r\n  exposed-modules:\r\n    Codec.Serialise\r\n    Codec.Serialise.Class\r\n    Codec.Serialise.Decoding\r\n    Codec.Serialise.Encoding\r\n    Codec.Serialise.IO\r\n    Codec.Serialise.Properties\r\n    Codec.Serialise.Tutorial\r\n    Codec.Serialise.Internal.GeneralisedUTF8\r\n\r\n  build-depends:\r\n    base                    >= 4.11    && < 4.18,\r\n    array                   >= 0.4     && < 0.6,\r\n    bytestring              >= 0.10.4  && < 0.12,\r\n    cborg                   == 0.2.*,\r\n    containers              >= 0.5     && < 0.7,\r\n    ghc-prim                >= 0.3.1.0 && < 0.10,\r\n    half                    >= 0.2.2.3 && < 0.4,\r\n    hashable                >= 1.2     && < 2.0,\r\n    primitive               >= 0.5     && < 0.8,\r\n    strict                  >= 0.4     && < 0.5,\r\n    text                    >= 1.1     && < 2.1,\r\n    these                   >= 1.1     && < 1.2,\r\n    unordered-containers    >= 0.2     && < 0.3,\r\n    vector                  >= 0.10    && < 0.14\r\n\r\n  if flag(newtime15)\r\n    build-depends:\r\n      time                  >= 1.5     && < 1.14\r\n  else\r\n    build-depends:\r\n      time                  >= 1.4     && < 1.5,\r\n      old-locale\r\n\r\n  if impl(ghc >= 8.0)\r\n    ghc-options: -Wcompat -Wnoncanonical-monad-instances\r\n\r\n--------------------------------------------------------------------------------\r\n-- Tests\r\n\r\ntest-suite tests\r\n  type:              exitcode-stdio-1.0\r\n  hs-source-dirs:    tests\r\n  main-is:           Main.hs\r\n\r\n  default-language:  Haskell2010\r\n  ghc-options:\r\n    -Wall -fno-warn-orphans\r\n    -threaded -rtsopts \"-with-rtsopts=-N2\"\r\n\r\n  other-modules:\r\n    Tests.IO\r\n    Tests.Negative\r\n    Tests.Orphanage\r\n    Tests.Serialise\r\n    Tests.Serialise.Canonical\r\n    Tests.Regress\r\n    Tests.Regress.Issue13\r\n    Tests.Regress.Issue67\r\n    Tests.Regress.Issue80\r\n    Tests.Regress.Issue106\r\n    Tests.Regress.Issue135\r\n    Tests.Deriving\r\n    Tests.GeneralisedUTF8\r\n\r\n  build-depends:\r\n    base                    >= 4.11    && < 4.18,\r\n    bytestring              >= 0.10.4  && < 0.12,\r\n    directory               >= 1.0     && < 1.4,\r\n    filepath                >= 1.0     && < 1.5,\r\n    text                    >= 1.1     && < 2.1,\r\n    time                    >= 1.4     && < 1.14,\r\n    containers              >= 0.5     && < 0.7,\r\n    unordered-containers    >= 0.2     && < 0.3,\r\n    primitive               >= 0.5     && < 0.8,\r\n    cborg,\r\n    serialise,\r\n    QuickCheck              >= 2.9     && < 2.15,\r\n    tasty                   >= 0.11    && < 1.5,\r\n    tasty-hunit             >= 0.9     && < 0.11,\r\n    tasty-quickcheck        >= 0.8     && < 0.11,\r\n    quickcheck-instances    >= 0.3.12  && < 0.4,\r\n    vector                  >= 0.10    && < 0.14\r\n\r\n--------------------------------------------------------------------------------\r\n-- Benchmarks\r\n\r\nbenchmark instances\r\n  type:              exitcode-stdio-1.0\r\n  hs-source-dirs:    bench/instances\r\n  main-is:           Main.hs\r\n\r\n  default-language:  Haskell2010\r\n  ghc-options:\r\n    -Wall -rtsopts -fno-cse -fno-ignore-asserts -fno-warn-orphans\r\n\r\n  other-modules:\r\n    Instances.Float\r\n    Instances.Integer\r\n    Instances.Vector\r\n    Instances.Time\r\n\r\n  build-depends:\r\n    base                    >= 4.11    && < 4.18,\r\n    binary                  >= 0.7     && < 0.11,\r\n    bytestring              >= 0.10.4  && < 0.12,\r\n    vector                  >= 0.10    && < 0.14,\r\n    cborg,\r\n    serialise,\r\n\r\n    deepseq                 >= 1.0     && < 1.5,\r\n    criterion               >= 1.0     && < 1.6\r\n\r\n  if flag(newtime15)\r\n    build-depends:\r\n      time                  >= 1.5     && < 1.14\r\n  else\r\n    build-depends:\r\n      time                  >= 1.4     && < 1.5,\r\n      old-locale\r\n\r\nbenchmark micro\r\n  type:              exitcode-stdio-1.0\r\n  hs-source-dirs:    bench/micro\r\n  main-is:           Main.hs\r\n\r\n  default-language:  Haskell2010\r\n  ghc-options:\r\n    -Wall -rtsopts -fno-cse -fno-ignore-asserts -fno-warn-orphans\r\n\r\n  other-modules:\r\n    Micro\r\n    Micro.Types\r\n    Micro.Load\r\n    Micro.DeepSeq\r\n    Micro.MemSize\r\n    Micro.ReadShow\r\n    Micro.PkgAesonGeneric\r\n    Micro.PkgAesonTH\r\n    Micro.PkgBinary\r\n    Micro.PkgCereal\r\n    Micro.PkgStore\r\n    Micro.CBOR\r\n\r\n    SimpleVersus\r\n\r\n  build-depends:\r\n    base                    >= 4.11    && < 4.18,\r\n    binary                  >= 0.7     && < 0.11,\r\n    bytestring              >= 0.10.4  && < 0.12,\r\n    ghc-prim                >= 0.3.1.0 && < 0.10,\r\n    vector                  >= 0.10    && < 0.14,\r\n    cborg,\r\n    serialise,\r\n\r\n    aeson                   >= 0.7     && < 2.2,\r\n    deepseq                 >= 1.0     && < 1.5,\r\n    criterion               >= 1.0     && < 1.6,\r\n    cereal                  >= 0.5.2.0 && < 0.6,\r\n    cereal-vector           >= 0.2     && < 0.3,\r\n    semigroups              >= 0.18    && < 0.21,\r\n    store                   >= 0.7.1   && < 0.8\r\n\r\nbenchmark versus\r\n  type:              exitcode-stdio-1.0\r\n  hs-source-dirs:    bench/versus\r\n  main-is:           Main.hs\r\n\r\n  default-language:  Haskell2010\r\n  ghc-options:\r\n    -Wall -rtsopts -fno-cse -fno-ignore-asserts -fno-warn-orphans\r\n\r\n  other-modules:\r\n    Utils\r\n\r\n    -- Suite #1\r\n    Mini\r\n\r\n    -- Suite #2\r\n    Macro\r\n    Macro.Types\r\n    Macro.Load\r\n    Macro.DeepSeq\r\n    Macro.MemSize\r\n    Macro.ReadShow\r\n    Macro.PkgAesonGeneric\r\n    Macro.PkgAesonTH\r\n    Macro.PkgBinary\r\n    Macro.PkgCereal\r\n    Macro.PkgStore\r\n    Macro.CBOR\r\n\r\n  build-depends:\r\n    base                    >= 4.11    && < 4.18,\r\n    array                   >= 0.4     && < 0.6,\r\n    binary                  >= 0.7     && < 0.11,\r\n    bytestring              >= 0.10.4  && < 0.12,\r\n    directory               >= 1.0     && < 1.4,\r\n    ghc-prim                >= 0.3.1.0 && < 0.10,\r\n    fail                    >= 4.9.0.0 && < 4.10,\r\n    text                    >= 1.1     && < 2.1,\r\n    vector                  >= 0.10    && < 0.14,\r\n    cborg,\r\n    serialise,\r\n\r\n    filepath                >= 1.0     && < 1.5,\r\n    containers              >= 0.5     && < 0.7,\r\n    deepseq                 >= 1.0     && < 1.5,\r\n    aeson                   >= 0.7     && < 2.2,\r\n    cereal                  >= 0.5.2.0 && < 0.6,\r\n    half                    >= 0.2.2.3 && < 0.4,\r\n    tar                     >= 0.4     && < 0.6,\r\n    zlib                    >= 0.5     && < 0.7,\r\n    pretty                  >= 1.0     && < 1.2,\r\n    criterion               >= 1.0     && < 1.6,\r\n    store                   >= 0.7.1   && < 0.8,\r\n    semigroups\r\n\r\n  if flag(newtime15)\r\n    build-depends:\r\n      time                  >= 1.5     && < 1.14\r\n  else\r\n    build-depends:\r\n      time                  >= 1.4     && < 1.5,\r\n      old-locale\r\n";
    }