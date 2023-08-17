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
    }) // {
    package-description-override = "Name:                tls\nVersion:             1.6.0\nDescription:\n   Native Haskell TLS and SSL protocol implementation for server and client.\n   .\n   This provides a high-level implementation of a sensitive security protocol,\n   eliminating a common set of security issues through the use of the advanced\n   type system, high level constructions and common Haskell features.\n   .\n   Currently implement the TLS1.0, TLS1.1, TLS1.2 and TLS 1.3 protocol,\n   and support RSA and Ephemeral (Elliptic curve and regular) Diffie Hellman key exchanges,\n   and many extensions.\n   .\n   Some debug tools linked with tls, are available through the\n   <http://hackage.haskell.org/package/tls-debug/>.\nLicense:             BSD3\nLicense-file:        LICENSE\nCopyright:           Vincent Hanquez <vincent@snarc.org>\nAuthor:              Vincent Hanquez <vincent@snarc.org>\nMaintainer:          Kazu Yamamoto <kazu@iij.ad.jp>\nSynopsis:            TLS/SSL protocol native implementation (Server and Client)\nBuild-Type:          Simple\nCategory:            Network\nstability:           experimental\nCabal-Version:       >=1.10\nHomepage:            http://github.com/vincenthz/hs-tls\nextra-source-files:  Tests/*.hs\n                     CHANGELOG.md\n\nFlag compat\n  Description:       Accept SSLv2 client hello for beginning SSLv3 / TLS handshake\n  Default:           True\n\nFlag network\n  Description:       Use the base network library\n  Default:           True\n\nFlag hans\n  Description:       Use the Haskell Network Stack (HaNS)\n  Default:           False\n\nLibrary\n  Default-Language:  Haskell2010\n  Build-Depends:     base >= 4.9 && < 5\n                   , mtl >= 2.2.1\n                   , transformers\n                   , cereal >= 0.5.3\n                   , bytestring\n                   , data-default-class\n                   -- crypto related\n                   , memory >= 0.14.6\n                   , cryptonite >= 0.27\n                   -- certificate related\n                   , asn1-types >= 0.2.0\n                   , asn1-encoding\n                   , x509 >= 1.7.5\n                   , x509-store >= 1.6\n                   , x509-validation >= 1.6.5\n                   , async >= 2.0\n                   , hourglass\n  if flag(network)\n    Build-Depends:   network >= 2.4.0.0\n    cpp-options:     -DINCLUDE_NETWORK\n  if flag(hans)\n    Build-Depends:   hans\n    cpp-options:     -DINCLUDE_HANS\n  Exposed-modules:   Network.TLS\n                     Network.TLS.Cipher\n                     Network.TLS.Compression\n                     Network.TLS.Internal\n                     Network.TLS.Extra\n                     Network.TLS.Extra.Cipher\n                     Network.TLS.Extra.FFDHE\n                     Network.TLS.QUIC\n  other-modules:     Network.TLS.Cap\n                     Network.TLS.Struct\n                     Network.TLS.Struct13\n                     Network.TLS.Core\n                     Network.TLS.Context\n                     Network.TLS.Context.Internal\n                     Network.TLS.Credentials\n                     Network.TLS.Backend\n                     Network.TLS.Crypto\n                     Network.TLS.Crypto.DH\n                     Network.TLS.Crypto.IES\n                     Network.TLS.Crypto.Types\n                     Network.TLS.ErrT\n                     Network.TLS.Extension\n                     Network.TLS.Handshake\n                     Network.TLS.Handshake.Certificate\n                     Network.TLS.Handshake.Client\n                     Network.TLS.Handshake.Common\n                     Network.TLS.Handshake.Common13\n                     Network.TLS.Handshake.Control\n                     Network.TLS.Handshake.Key\n                     Network.TLS.Handshake.Process\n                     Network.TLS.Handshake.Random\n                     Network.TLS.Handshake.Server\n                     Network.TLS.Handshake.Signature\n                     Network.TLS.Handshake.State\n                     Network.TLS.Handshake.State13\n                     Network.TLS.Hooks\n                     Network.TLS.IO\n                     Network.TLS.Imports\n                     Network.TLS.KeySchedule\n                     Network.TLS.MAC\n                     Network.TLS.Measurement\n                     Network.TLS.Packet\n                     Network.TLS.Packet13\n                     Network.TLS.Parameters\n                     Network.TLS.PostHandshake\n                     Network.TLS.Record\n                     Network.TLS.Record.Disengage\n                     Network.TLS.Record.Engage\n                     Network.TLS.Record.Layer\n                     Network.TLS.Record.Reading\n                     Network.TLS.Record.Writing\n                     Network.TLS.Record.State\n                     Network.TLS.Record.Types\n                     Network.TLS.RNG\n                     Network.TLS.State\n                     Network.TLS.Session\n                     Network.TLS.Sending\n                     Network.TLS.Receiving\n                     Network.TLS.Util\n                     Network.TLS.Util.ASN1\n                     Network.TLS.Util.Serialization\n                     Network.TLS.Types\n                     Network.TLS.Wire\n                     Network.TLS.X509\n  ghc-options:       -Wall\n  if flag(compat)\n    cpp-options:     -DSSLV2_COMPATIBLE\n\nTest-Suite test-tls\n  Default-Language:  Haskell2010\n  type:              exitcode-stdio-1.0\n  hs-source-dirs:    Tests\n  Main-is:           Tests.hs\n  other-modules:     Certificate\n                     Ciphers\n                     Connection\n                     Marshalling\n                     PipeChan\n                     PubKey\n  Build-Depends:     base >= 3 && < 5\n                   , async >= 2.0\n                   , data-default-class\n                   , tasty\n                   , tasty-quickcheck\n                   , tls\n                   , QuickCheck\n                   , cryptonite\n                   , bytestring\n                   , asn1-types\n                   , x509\n                   , x509-validation\n                   , hourglass\n  ghc-options:       -Wall -fno-warn-unused-imports\n\nBenchmark bench-tls\n  Default-Language:  Haskell2010\n  hs-source-dirs:    Benchmarks Tests\n  Main-Is:           Benchmarks.hs\n  type:              exitcode-stdio-1.0\n  other-modules:     Certificate\n                     Connection\n                     PipeChan\n                     PubKey\n  Build-depends:     base >= 4 && < 5\n                   , tls\n                   , x509\n                   , x509-validation\n                   , data-default-class\n                   , cryptonite\n                   , gauge\n                   , bytestring\n                   , asn1-types\n                   , async >= 2.0\n                   , hourglass\n                   , QuickCheck >= 2\n                   , tasty-quickcheck\n                   , tls\n  ghc-options:       -Wall -fno-warn-unused-imports\n\nsource-repository head\n  type: git\n  location: https://github.com/vincenthz/hs-tls\n  subdir: core\n";
    }