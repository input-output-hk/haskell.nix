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
      identifier = { name = "x509-validation"; version = "1.6.12"; };
      license = "BSD-3-Clause";
      copyright = "Vincent Hanquez <vincent@snarc.org>";
      maintainer = "Vincent Hanquez <vincent@snarc.org>";
      author = "Vincent Hanquez <vincent@snarc.org>";
      homepage = "http://github.com/vincenthz/hs-certificate";
      url = "";
      synopsis = "X.509 Certificate and CRL validation";
      description = "X.509 Certificate and CRL validation. please see README";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."memory" or (errorHandler.buildDepError "memory"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."hourglass" or (errorHandler.buildDepError "hourglass"))
          (hsPkgs."data-default-class" or (errorHandler.buildDepError "data-default-class"))
          (hsPkgs."pem" or (errorHandler.buildDepError "pem"))
          (hsPkgs."asn1-types" or (errorHandler.buildDepError "asn1-types"))
          (hsPkgs."asn1-encoding" or (errorHandler.buildDepError "asn1-encoding"))
          (hsPkgs."x509" or (errorHandler.buildDepError "x509"))
          (hsPkgs."x509-store" or (errorHandler.buildDepError "x509-store"))
          (hsPkgs."cryptonite" or (errorHandler.buildDepError "cryptonite"))
          ];
        buildable = true;
        };
      tests = {
        "test-x509-validation" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."memory" or (errorHandler.buildDepError "memory"))
            (hsPkgs."data-default-class" or (errorHandler.buildDepError "data-default-class"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."hourglass" or (errorHandler.buildDepError "hourglass"))
            (hsPkgs."asn1-types" or (errorHandler.buildDepError "asn1-types"))
            (hsPkgs."asn1-encoding" or (errorHandler.buildDepError "asn1-encoding"))
            (hsPkgs."x509" or (errorHandler.buildDepError "x509"))
            (hsPkgs."x509-store" or (errorHandler.buildDepError "x509-store"))
            (hsPkgs."x509-validation" or (errorHandler.buildDepError "x509-validation"))
            (hsPkgs."cryptonite" or (errorHandler.buildDepError "cryptonite"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/x509-validation-1.6.12.tar.gz";
      sha256 = "0d8e44e199332b22df3e7c19d21b1a79f237fde9a3abf23bef9e7c4991d0f1c8";
      });
    }