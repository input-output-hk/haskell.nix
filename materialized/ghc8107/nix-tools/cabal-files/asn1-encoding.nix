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
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = { name = "asn1-encoding"; version = "0.9.6"; };
      license = "BSD-3-Clause";
      copyright = "Vincent Hanquez <vincent@snarc.org>";
      maintainer = "vincent@snarc.org";
      author = "Vincent Hanquez <vincent@snarc.org>";
      homepage = "https://github.com/vincenthz/hs-asn1";
      url = "";
      synopsis = "ASN1 data reader and writer in RAW, BER and DER forms";
      description = "ASN1 data reader and writer in raw form with supports for high level forms of ASN1 (BER, and DER).";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."hourglass" or (errorHandler.buildDepError "hourglass"))
          (hsPkgs."asn1-types" or (errorHandler.buildDepError "asn1-types"))
          ];
        buildable = true;
        };
      tests = {
        "tests-asn1-encoding" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."asn1-types" or (errorHandler.buildDepError "asn1-types"))
            (hsPkgs."asn1-encoding" or (errorHandler.buildDepError "asn1-encoding"))
            (hsPkgs."hourglass" or (errorHandler.buildDepError "hourglass"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/asn1-encoding-0.9.6.tar.gz";
      sha256 = "d9f8deabd3b908e5cf83c0d813c08dc0143b3ec1c0d97f660d2cfa02c1c8da0a";
      });
    }