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
      identifier = { name = "crypton-x509-store"; version = "1.6.9"; };
      license = "BSD-3-Clause";
      copyright = "Vincent Hanquez <vincent@snarc.org>";
      maintainer = "Kazu Yamamoto <kazu@iij.ad.jp>";
      author = "Vincent Hanquez <vincent@snarc.org>";
      homepage = "https://github.com/kazu-yamamoto/crypton-certificate";
      url = "";
      synopsis = "X.509 collection accessing and storing methods";
      description = "X.509 collection accessing and storing methods for certificate, crl, exception list";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."pem" or (errorHandler.buildDepError "pem"))
          (hsPkgs."asn1-types" or (errorHandler.buildDepError "asn1-types"))
          (hsPkgs."asn1-encoding" or (errorHandler.buildDepError "asn1-encoding"))
          (hsPkgs."crypton" or (errorHandler.buildDepError "crypton"))
          (hsPkgs."crypton-x509" or (errorHandler.buildDepError "crypton-x509"))
          ];
        buildable = true;
        };
      tests = {
        "test-x509-store" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."crypton-x509" or (errorHandler.buildDepError "crypton-x509"))
            (hsPkgs."crypton-x509-store" or (errorHandler.buildDepError "crypton-x509-store"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/crypton-x509-store-1.6.9.tar.gz";
      sha256 = "3e6218af12e039cc291d19792db044df1647b5cf0bbc60b909a027e7595a256f";
      });
    }) // {
    package-description-override = "Name:                crypton-x509-store\nversion:             1.6.9\nDescription:         X.509 collection accessing and storing methods for certificate, crl, exception list\nLicense:             BSD3\nLicense-file:        LICENSE\nCopyright:           Vincent Hanquez <vincent@snarc.org>\nAuthor:              Vincent Hanquez <vincent@snarc.org>\nMaintainer:          Kazu Yamamoto <kazu@iij.ad.jp>\nSynopsis:            X.509 collection accessing and storing methods\nBuild-Type:          Simple\nCategory:            Data\nstability:           experimental\nHomepage:            https://github.com/kazu-yamamoto/crypton-certificate\nCabal-Version:       >= 1.10\n\nLibrary\n  Default-Language:  Haskell2010\n  Build-Depends:     base >= 3 && < 5\n                   , bytestring\n                   , mtl\n                   , containers\n                   , directory\n                   , filepath\n                   , pem >= 0.1 && < 0.3\n                   , asn1-types >= 0.3 && < 0.4\n                   , asn1-encoding >= 0.9 && < 0.10\n                   , crypton\n                   , crypton-x509 >= 1.7.2\n  Exposed-modules:   Data.X509.CertificateStore\n                     Data.X509.File\n                     Data.X509.Memory\n  ghc-options:       -Wall\n\nTest-Suite test-x509-store\n  Default-Language:  Haskell2010\n  type:              exitcode-stdio-1.0\n  hs-source-dirs:    Tests\n  Main-is:           Tests.hs\n  Build-Depends:     base >= 3 && < 5\n                   , bytestring\n                   , tasty\n                   , tasty-hunit\n                   , crypton-x509\n                   , crypton-x509-store\n  ghc-options:       -Wall\n\nsource-repository head\n  type:     git\n  location: git://github.com/vincenthz/hs-certificate\n  subdir:   x509-store\n";
    }