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
      identifier = { name = "x509-store"; version = "1.6.9"; };
      license = "BSD-3-Clause";
      copyright = "Vincent Hanquez <vincent@snarc.org>";
      maintainer = "Vincent Hanquez <vincent@snarc.org>";
      author = "Vincent Hanquez <vincent@snarc.org>";
      homepage = "http://github.com/vincenthz/hs-certificate";
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
          (hsPkgs."cryptonite" or (errorHandler.buildDepError "cryptonite"))
          (hsPkgs."x509" or (errorHandler.buildDepError "x509"))
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
            (hsPkgs."x509" or (errorHandler.buildDepError "x509"))
            (hsPkgs."x509-store" or (errorHandler.buildDepError "x509-store"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/x509-store-1.6.9.tar.gz";
      sha256 = "c59213520cf31a0a18611a60b8a4d2d7aa6cb206c0545d857b98dcb90fc5c8da";
      });
    }) // {
    package-description-override = "Name:                x509-store\nversion:             1.6.9\nDescription:         X.509 collection accessing and storing methods for certificate, crl, exception list\nLicense:             BSD3\nLicense-file:        LICENSE\nCopyright:           Vincent Hanquez <vincent@snarc.org>\nAuthor:              Vincent Hanquez <vincent@snarc.org>\nMaintainer:          Vincent Hanquez <vincent@snarc.org>\nSynopsis:            X.509 collection accessing and storing methods\nBuild-Type:          Simple\nCategory:            Data\nstability:           experimental\nHomepage:            http://github.com/vincenthz/hs-certificate\nCabal-Version:       >= 1.10\n\nLibrary\n  Default-Language:  Haskell2010\n  Build-Depends:     base >= 3 && < 5\n                   , bytestring\n                   , mtl\n                   , containers\n                   , directory\n                   , filepath\n                   , pem >= 0.1 && < 0.3\n                   , asn1-types >= 0.3 && < 0.4\n                   , asn1-encoding >= 0.9 && < 0.10\n                   , cryptonite\n                   , x509 >= 1.7.2\n  Exposed-modules:   Data.X509.CertificateStore\n                     Data.X509.File\n                     Data.X509.Memory\n  ghc-options:       -Wall\n\nTest-Suite test-x509-store\n  Default-Language:  Haskell2010\n  type:              exitcode-stdio-1.0\n  hs-source-dirs:    Tests\n  Main-is:           Tests.hs\n  Build-Depends:     base >= 3 && < 5\n                   , bytestring\n                   , tasty\n                   , tasty-hunit\n                   , x509\n                   , x509-store\n  ghc-options:       -Wall\n\nsource-repository head\n  type:     git\n  location: git://github.com/vincenthz/hs-certificate\n  subdir:   x509-store\n";
    }