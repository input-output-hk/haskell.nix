{ system
  , compiler
  , flags
  , pkgs
  , hsPkgs
  , pkgconfPkgs
  , errorHandler
  , config
  , ... }:
  {
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
    }