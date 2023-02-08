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
    }) // {
    package-description-override = "Name:                x509-validation\nversion:             1.6.12\nDescription:         X.509 Certificate and CRL validation. please see README\nLicense:             BSD3\nLicense-file:        LICENSE\nCopyright:           Vincent Hanquez <vincent@snarc.org>\nAuthor:              Vincent Hanquez <vincent@snarc.org>\nMaintainer:          Vincent Hanquez <vincent@snarc.org>\nSynopsis:            X.509 Certificate and CRL validation\nBuild-Type:          Simple\nCategory:            Data\nstability:           experimental\nHomepage:            http://github.com/vincenthz/hs-certificate\nCabal-Version:       >= 1.10\n\nLibrary\n  Default-Language:  Haskell2010\n  Build-Depends:     base >= 3 && < 5\n                   , bytestring\n                   , memory\n                   , mtl\n                   , containers\n                   , hourglass\n                   , data-default-class\n                   , pem >= 0.1\n                   , asn1-types >= 0.3 && < 0.4\n                   , asn1-encoding >= 0.9 && < 0.10\n                   , x509 >= 1.7.5\n                   , x509-store >= 1.6\n                   , cryptonite >= 0.24\n  Exposed-modules:   Data.X509.Validation\n  Other-modules:     Data.X509.Validation.Signature\n                     Data.X509.Validation.Fingerprint\n                     Data.X509.Validation.Cache\n                     Data.X509.Validation.Types\n  ghc-options:       -Wall\n\nTest-Suite test-x509-validation\n  Default-Language:  Haskell2010\n  type:              exitcode-stdio-1.0\n  hs-source-dirs:    Tests\n  Main-is:           Tests.hs\n  Other-modules:     Certificate\n  Build-Depends:     base >= 3 && < 5\n                   , bytestring\n                   , memory\n                   , data-default-class\n                   , tasty\n                   , tasty-hunit\n                   , hourglass\n                   , asn1-types\n                   , asn1-encoding\n                   , x509 >= 1.7.1\n                   , x509-store\n                   , x509-validation\n                   , cryptonite\n  ghc-options:       -Wall\n\nsource-repository head\n  type:     git\n  location: git://github.com/vincenthz/hs-certificate\n  subdir:   x509-validation\n";
    }