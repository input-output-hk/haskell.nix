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
      specVersion = "1.6";
      identifier = { name = "connection"; version = "0.3.1"; };
      license = "BSD-3-Clause";
      copyright = "Vincent Hanquez <vincent@snarc.org>";
      maintainer = "Vincent Hanquez <vincent@snarc.org>";
      author = "Vincent Hanquez <vincent@snarc.org>";
      homepage = "https://github.com/vincenthz/hs-connection";
      url = "";
      synopsis = "Simple and easy network connections API";
      description = "Simple network library for all your connection need.\n\nFeatures: Really simple to use, SSL/TLS, SOCKS.\n\nThis library provides a very simple api to create sockets\nto a destination with the choice of SSL/TLS, and SOCKS.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."basement" or (errorHandler.buildDepError "basement"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."data-default-class" or (errorHandler.buildDepError "data-default-class"))
          (hsPkgs."network" or (errorHandler.buildDepError "network"))
          (hsPkgs."tls" or (errorHandler.buildDepError "tls"))
          (hsPkgs."socks" or (errorHandler.buildDepError "socks"))
          (hsPkgs."x509" or (errorHandler.buildDepError "x509"))
          (hsPkgs."x509-store" or (errorHandler.buildDepError "x509-store"))
          (hsPkgs."x509-system" or (errorHandler.buildDepError "x509-system"))
          (hsPkgs."x509-validation" or (errorHandler.buildDepError "x509-validation"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/connection-0.3.1.tar.gz";
      sha256 = "5d759589c532c34d87bfc4f6fcb732bf55b55a93559d3b94229e8347a15375d9";
      });
    }) // {
    package-description-override = "Name:                connection\r\nVersion:             0.3.1\r\nx-revision: 1\r\nDescription:\r\n    Simple network library for all your connection need.\r\n    .\r\n    Features: Really simple to use, SSL/TLS, SOCKS.\r\n    .\r\n    This library provides a very simple api to create sockets\r\n    to a destination with the choice of SSL/TLS, and SOCKS.\r\nLicense:             BSD3\r\nLicense-file:        LICENSE\r\nCopyright:           Vincent Hanquez <vincent@snarc.org>\r\nAuthor:              Vincent Hanquez <vincent@snarc.org>\r\nMaintainer:          Vincent Hanquez <vincent@snarc.org>\r\nSynopsis:            Simple and easy network connections API\r\nBuild-Type:          Simple\r\nCategory:            Network\r\nstability:           experimental\r\nCabal-Version:       >=1.6\r\nHomepage:            https://github.com/vincenthz/hs-connection\r\nextra-source-files:  README.md\r\n                     CHANGELOG.md\r\n\r\nLibrary\r\n  Build-Depends:     base >= 4.8 && < 5\r\n                   , basement\r\n                   , bytestring\r\n                   , containers\r\n                   , data-default-class\r\n                   , network >= 2.6.3\r\n                   , tls >= 1.4\r\n                   , socks >= 0.6\r\n                   , x509 >= 1.5\r\n                   , x509-store >= 1.5\r\n                   , x509-system >= 1.5\r\n                   , x509-validation >= 1.5\r\n  Exposed-modules:   Network.Connection\r\n  Other-modules:     Network.Connection.Types\r\n  ghc-options:       -Wall\r\n\r\nsource-repository head\r\n  type: git\r\n  location: https://github.com/vincenthz/hs-connection\r\n";
    }