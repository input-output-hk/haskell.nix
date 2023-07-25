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
      identifier = { name = "crypton-connection"; version = "0.3.1"; };
      license = "BSD-3-Clause";
      copyright = "Vincent Hanquez <vincent@snarc.org>";
      maintainer = "Kazu Yamamoto <kazu@iij.ad.jp>";
      author = "Vincent Hanquez <vincent@snarc.org>";
      homepage = "https://github.com/kazu-yamamoto/crypton-connection";
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
          (hsPkgs."crypton-x509" or (errorHandler.buildDepError "crypton-x509"))
          (hsPkgs."crypton-x509-store" or (errorHandler.buildDepError "crypton-x509-store"))
          (hsPkgs."crypton-x509-system" or (errorHandler.buildDepError "crypton-x509-system"))
          (hsPkgs."crypton-x509-validation" or (errorHandler.buildDepError "crypton-x509-validation"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/crypton-connection-0.3.1.tar.gz";
      sha256 = "03073694f6525f975db78dbc67c34e89bb27b95cf863d4a3a77dc6ef49d81dcc";
      });
    }) // {
    package-description-override = "Name:                crypton-connection\nVersion:             0.3.1\nDescription:\n    Simple network library for all your connection need.\n    .\n    Features: Really simple to use, SSL/TLS, SOCKS.\n    .\n    This library provides a very simple api to create sockets\n    to a destination with the choice of SSL/TLS, and SOCKS.\nLicense:             BSD3\nLicense-file:        LICENSE\nCopyright:           Vincent Hanquez <vincent@snarc.org>\nAuthor:              Vincent Hanquez <vincent@snarc.org>\nMaintainer:          Kazu Yamamoto <kazu@iij.ad.jp>\nSynopsis:            Simple and easy network connections API\nBuild-Type:          Simple\nCategory:            Network\nstability:           experimental\nCabal-Version:       >=1.10\nHomepage:            https://github.com/kazu-yamamoto/crypton-connection\nextra-source-files:  README.md\n                     CHANGELOG.md\n\nLibrary\n  Default-Language:  Haskell2010\n  Build-Depends:     base >= 3 && < 5\n                   , basement\n                   , bytestring\n                   , containers\n                   , data-default-class\n                   , network >= 2.6.3\n                   , tls >= 1.7\n                   , socks >= 0.6\n                   , crypton-x509 >= 1.5\n                   , crypton-x509-store >= 1.5\n                   , crypton-x509-system >= 1.5\n                   , crypton-x509-validation >= 1.5\n  Exposed-modules:   Network.Connection\n  Other-modules:     Network.Connection.Types\n  ghc-options:       -Wall\n\nsource-repository head\n  type: git\n  location: https://github.com/kazu-yamamoto/crypton-connection\n";
    }