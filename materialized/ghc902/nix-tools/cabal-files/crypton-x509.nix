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
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = { name = "crypton-x509"; version = "1.7.6"; };
      license = "BSD-3-Clause";
      copyright = "Vincent Hanquez <vincent@snarc.org>";
      maintainer = "Kazu Yamamoto <kazu@iij.ad.jp>";
      author = "Vincent Hanquez <vincent@snarc.org>";
      homepage = "https://github.com/kazu-yamamoto/crypton-certificate";
      url = "";
      synopsis = "X509 reader and writer";
      description = "X509 reader and writer. please see README";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."memory" or (errorHandler.buildDepError "memory"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."hourglass" or (errorHandler.buildDepError "hourglass"))
          (hsPkgs."pem" or (errorHandler.buildDepError "pem"))
          (hsPkgs."asn1-types" or (errorHandler.buildDepError "asn1-types"))
          (hsPkgs."asn1-encoding" or (errorHandler.buildDepError "asn1-encoding"))
          (hsPkgs."asn1-parse" or (errorHandler.buildDepError "asn1-parse"))
          (hsPkgs."crypton" or (errorHandler.buildDepError "crypton"))
          ];
        buildable = true;
        };
      tests = {
        "test-x509" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."hourglass" or (errorHandler.buildDepError "hourglass"))
            (hsPkgs."asn1-types" or (errorHandler.buildDepError "asn1-types"))
            (hsPkgs."x509" or (errorHandler.buildDepError "x509"))
            (hsPkgs."crypton" or (errorHandler.buildDepError "crypton"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/crypton-x509-1.7.6.tar.gz";
      sha256 = "ebb74aca2d00261e2fb4927d211ba1a174e190e5257f309e190f019727f8caff";
      });
    }) // {
    package-description-override = "Name:                crypton-x509\r\nversion:             1.7.6\r\nx-revision: 1\r\nDescription:         X509 reader and writer. please see README\r\nLicense:             BSD3\r\nLicense-file:        LICENSE\r\nCopyright:           Vincent Hanquez <vincent@snarc.org>\r\nAuthor:              Vincent Hanquez <vincent@snarc.org>\r\nMaintainer:          Kazu Yamamoto <kazu@iij.ad.jp>\r\nSynopsis:            X509 reader and writer\r\nBuild-Type:          Simple\r\nCategory:            Data\r\nstability:           experimental\r\nHomepage:            https://github.com/kazu-yamamoto/crypton-certificate\r\nCabal-Version:       >= 1.10\r\n\r\nLibrary\r\n  Default-Language:  Haskell2010\r\n  Build-Depends:     base >= 4.7 && < 5\r\n                   , bytestring\r\n                   , memory\r\n                   , transformers >= 0.4\r\n                   , containers\r\n                   , hourglass\r\n                   , pem >= 0.1\r\n                   , asn1-types >= 0.3.1 && < 0.4\r\n                   , asn1-encoding >= 0.9 && < 0.10\r\n                   , asn1-parse >= 0.9.3 && < 0.10\r\n                   , crypton >= 0.24\r\n  Exposed-modules:   Data.X509\r\n                     Data.X509.EC\r\n  Other-modules:     Data.X509.Internal\r\n                     Data.X509.CertificateChain\r\n                     Data.X509.AlgorithmIdentifier\r\n                     Data.X509.DistinguishedName\r\n                     Data.X509.Cert\r\n                     Data.X509.PublicKey\r\n                     Data.X509.PrivateKey\r\n                     Data.X509.Ext\r\n                     Data.X509.ExtensionRaw\r\n                     Data.X509.CRL\r\n                     Data.X509.OID\r\n                     Data.X509.Signed\r\n  ghc-options:       -Wall\r\n\r\nTest-Suite test-x509\r\n  Default-Language:  Haskell2010\r\n  type:              exitcode-stdio-1.0\r\n  hs-source-dirs:    Tests\r\n  Main-is:           Tests.hs\r\n  Build-Depends:     base >= 3 && < 5\r\n                   , bytestring\r\n                   , mtl\r\n                   , tasty\r\n                   , tasty-quickcheck\r\n                   , hourglass\r\n                   , asn1-types\r\n                   , x509\r\n                   , crypton\r\n  ghc-options:       -Wall -fno-warn-orphans -fno-warn-missing-signatures\r\n\r\nsource-repository head\r\n  type:     git\r\n  location: https://github.com/kazu-yamamoto/crypton-certificate\r\n  subdir:   x509\r\n";
    }