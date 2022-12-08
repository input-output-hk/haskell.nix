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
    flags = { compat = true; network = true; hans = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "tls"; version = "1.6.0"; };
      license = "BSD-3-Clause";
      copyright = "Vincent Hanquez <vincent@snarc.org>";
      maintainer = "Kazu Yamamoto <kazu@iij.ad.jp>";
      author = "Vincent Hanquez <vincent@snarc.org>";
      homepage = "http://github.com/vincenthz/hs-tls";
      url = "";
      synopsis = "TLS/SSL protocol native implementation (Server and Client)";
      description = "Native Haskell TLS and SSL protocol implementation for server and client.\n\nThis provides a high-level implementation of a sensitive security protocol,\neliminating a common set of security issues through the use of the advanced\ntype system, high level constructions and common Haskell features.\n\nCurrently implement the TLS1.0, TLS1.1, TLS1.2 and TLS 1.3 protocol,\nand support RSA and Ephemeral (Elliptic curve and regular) Diffie Hellman key exchanges,\nand many extensions.\n\nSome debug tools linked with tls, are available through the\n<http://hackage.haskell.org/package/tls-debug/>.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = ([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."cereal" or (errorHandler.buildDepError "cereal"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."data-default-class" or (errorHandler.buildDepError "data-default-class"))
          (hsPkgs."memory" or (errorHandler.buildDepError "memory"))
          (hsPkgs."cryptonite" or (errorHandler.buildDepError "cryptonite"))
          (hsPkgs."asn1-types" or (errorHandler.buildDepError "asn1-types"))
          (hsPkgs."asn1-encoding" or (errorHandler.buildDepError "asn1-encoding"))
          (hsPkgs."x509" or (errorHandler.buildDepError "x509"))
          (hsPkgs."x509-store" or (errorHandler.buildDepError "x509-store"))
          (hsPkgs."x509-validation" or (errorHandler.buildDepError "x509-validation"))
          (hsPkgs."async" or (errorHandler.buildDepError "async"))
          (hsPkgs."hourglass" or (errorHandler.buildDepError "hourglass"))
          ] ++ (pkgs.lib).optional (flags.network) (hsPkgs."network" or (errorHandler.buildDepError "network"))) ++ (pkgs.lib).optional (flags.hans) (hsPkgs."hans" or (errorHandler.buildDepError "hans"));
        buildable = true;
        };
      tests = {
        "test-tls" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."async" or (errorHandler.buildDepError "async"))
            (hsPkgs."data-default-class" or (errorHandler.buildDepError "data-default-class"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."tls" or (errorHandler.buildDepError "tls"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."cryptonite" or (errorHandler.buildDepError "cryptonite"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."asn1-types" or (errorHandler.buildDepError "asn1-types"))
            (hsPkgs."x509" or (errorHandler.buildDepError "x509"))
            (hsPkgs."x509-validation" or (errorHandler.buildDepError "x509-validation"))
            (hsPkgs."hourglass" or (errorHandler.buildDepError "hourglass"))
            ];
          buildable = true;
          };
        };
      benchmarks = {
        "bench-tls" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."tls" or (errorHandler.buildDepError "tls"))
            (hsPkgs."x509" or (errorHandler.buildDepError "x509"))
            (hsPkgs."x509-validation" or (errorHandler.buildDepError "x509-validation"))
            (hsPkgs."data-default-class" or (errorHandler.buildDepError "data-default-class"))
            (hsPkgs."cryptonite" or (errorHandler.buildDepError "cryptonite"))
            (hsPkgs."gauge" or (errorHandler.buildDepError "gauge"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."asn1-types" or (errorHandler.buildDepError "asn1-types"))
            (hsPkgs."async" or (errorHandler.buildDepError "async"))
            (hsPkgs."hourglass" or (errorHandler.buildDepError "hourglass"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."tls" or (errorHandler.buildDepError "tls"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/tls-1.6.0.tar.gz";
      sha256 = "d29d20afc1c1b852a3c39b076ec853cb596ac888cf521e54144441dec689e498";
      });
    }