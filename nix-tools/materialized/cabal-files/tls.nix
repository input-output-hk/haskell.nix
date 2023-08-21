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
      identifier = { name = "tls"; version = "1.7.0"; };
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
          (hsPkgs."crypton" or (errorHandler.buildDepError "crypton"))
          (hsPkgs."asn1-types" or (errorHandler.buildDepError "asn1-types"))
          (hsPkgs."asn1-encoding" or (errorHandler.buildDepError "asn1-encoding"))
          (hsPkgs."crypton-x509" or (errorHandler.buildDepError "crypton-x509"))
          (hsPkgs."crypton-x509-store" or (errorHandler.buildDepError "crypton-x509-store"))
          (hsPkgs."crypton-x509-validation" or (errorHandler.buildDepError "crypton-x509-validation"))
          (hsPkgs."async" or (errorHandler.buildDepError "async"))
          (hsPkgs."unix-time" or (errorHandler.buildDepError "unix-time"))
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
            (hsPkgs."crypton" or (errorHandler.buildDepError "crypton"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."asn1-types" or (errorHandler.buildDepError "asn1-types"))
            (hsPkgs."crypton-x509" or (errorHandler.buildDepError "crypton-x509"))
            (hsPkgs."crypton-x509-validation" or (errorHandler.buildDepError "crypton-x509-validation"))
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
            (hsPkgs."crypton-x509" or (errorHandler.buildDepError "crypton-x509"))
            (hsPkgs."crypton-x509-validation" or (errorHandler.buildDepError "crypton-x509-validation"))
            (hsPkgs."data-default-class" or (errorHandler.buildDepError "data-default-class"))
            (hsPkgs."crypton" or (errorHandler.buildDepError "crypton"))
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
      url = "http://hackage.haskell.org/package/tls-1.7.0.tar.gz";
      sha256 = "3c2a50c902d26864f6af113e59045f049f6c54fb46239ed2d1d4a82bc8524078";
      });
    }) // {
    package-description-override = "cabal-version:      >=1.10\nname:               tls\nversion:            1.7.0\nlicense:            BSD3\nlicense-file:       LICENSE\ncopyright:          Vincent Hanquez <vincent@snarc.org>\nmaintainer:         Kazu Yamamoto <kazu@iij.ad.jp>\nauthor:             Vincent Hanquez <vincent@snarc.org>\nstability:          experimental\nhomepage:           http://github.com/vincenthz/hs-tls\nsynopsis:           TLS/SSL protocol native implementation (Server and Client)\ndescription:\n    Native Haskell TLS and SSL protocol implementation for server and client.\n    .\n    This provides a high-level implementation of a sensitive security protocol,\n    eliminating a common set of security issues through the use of the advanced\n    type system, high level constructions and common Haskell features.\n    .\n    Currently implement the TLS1.0, TLS1.1, TLS1.2 and TLS 1.3 protocol,\n    and support RSA and Ephemeral (Elliptic curve and regular) Diffie Hellman key exchanges,\n    and many extensions.\n    .\n    Some debug tools linked with tls, are available through the\n    <http://hackage.haskell.org/package/tls-debug/>.\n\ncategory:           Network\nbuild-type:         Simple\nextra-source-files:\n    Tests/*.hs\n    CHANGELOG.md\n\nsource-repository head\n    type:     git\n    location: https://github.com/vincenthz/hs-tls\n    subdir:   core\n\nflag compat\n    description:\n        Accept SSLv2 client hello for beginning SSLv3 / TLS handshake\n\nflag network\n    description: Use the base network library\n\nflag hans\n    description: Use the Haskell Network Stack (HaNS)\n    default:     False\n\nlibrary\n    exposed-modules:\n        Network.TLS\n        Network.TLS.Cipher\n        Network.TLS.Compression\n        Network.TLS.Internal\n        Network.TLS.Extra\n        Network.TLS.Extra.Cipher\n        Network.TLS.Extra.FFDHE\n        Network.TLS.QUIC\n\n    other-modules:\n        Network.TLS.Cap\n        Network.TLS.Struct\n        Network.TLS.Struct13\n        Network.TLS.Core\n        Network.TLS.Context\n        Network.TLS.Context.Internal\n        Network.TLS.Credentials\n        Network.TLS.Backend\n        Network.TLS.Crypto\n        Network.TLS.Crypto.DH\n        Network.TLS.Crypto.IES\n        Network.TLS.Crypto.Types\n        Network.TLS.ErrT\n        Network.TLS.Extension\n        Network.TLS.Handshake\n        Network.TLS.Handshake.Certificate\n        Network.TLS.Handshake.Client\n        Network.TLS.Handshake.Common\n        Network.TLS.Handshake.Common13\n        Network.TLS.Handshake.Control\n        Network.TLS.Handshake.Key\n        Network.TLS.Handshake.Process\n        Network.TLS.Handshake.Random\n        Network.TLS.Handshake.Server\n        Network.TLS.Handshake.Signature\n        Network.TLS.Handshake.State\n        Network.TLS.Handshake.State13\n        Network.TLS.Hooks\n        Network.TLS.IO\n        Network.TLS.Imports\n        Network.TLS.KeySchedule\n        Network.TLS.MAC\n        Network.TLS.Measurement\n        Network.TLS.Packet\n        Network.TLS.Packet13\n        Network.TLS.Parameters\n        Network.TLS.PostHandshake\n        Network.TLS.Record\n        Network.TLS.Record.Disengage\n        Network.TLS.Record.Engage\n        Network.TLS.Record.Layer\n        Network.TLS.Record.Reading\n        Network.TLS.Record.Writing\n        Network.TLS.Record.State\n        Network.TLS.Record.Types\n        Network.TLS.RNG\n        Network.TLS.State\n        Network.TLS.Session\n        Network.TLS.Sending\n        Network.TLS.Receiving\n        Network.TLS.Util\n        Network.TLS.Util.ASN1\n        Network.TLS.Util.Serialization\n        Network.TLS.Types\n        Network.TLS.Wire\n        Network.TLS.X509\n\n    default-language: Haskell2010\n    ghc-options:      -Wall\n    build-depends:\n        base >=4.9 && <5,\n        mtl >=2.2.1,\n        transformers,\n        cereal >=0.5.3,\n        bytestring,\n        data-default-class,\n        memory >=0.14.6,\n        crypton,\n        asn1-types >=0.2.0,\n        asn1-encoding,\n        crypton-x509 >=1.7.5,\n        crypton-x509-store >=1.6,\n        crypton-x509-validation >=1.6.5,\n        async >=2.0,\n        unix-time\n\n    if flag(network)\n        cpp-options:   -DINCLUDE_NETWORK\n        build-depends: network >=2.4.0.0\n\n    if flag(hans)\n        cpp-options:   -DINCLUDE_HANS\n        build-depends: hans\n\n    if flag(compat)\n        cpp-options: -DSSLV2_COMPATIBLE\n\ntest-suite test-tls\n    type:             exitcode-stdio-1.0\n    main-is:          Tests.hs\n    hs-source-dirs:   Tests\n    other-modules:\n        Certificate\n        Ciphers\n        Connection\n        Marshalling\n        PipeChan\n        PubKey\n\n    default-language: Haskell2010\n    ghc-options:      -Wall -fno-warn-unused-imports\n    build-depends:\n        base >=3 && <5,\n        async >=2.0,\n        data-default-class,\n        tasty,\n        tasty-quickcheck,\n        tls,\n        QuickCheck,\n        crypton,\n        bytestring,\n        asn1-types,\n        crypton-x509,\n        crypton-x509-validation,\n        hourglass\n\nbenchmark bench-tls\n    type:             exitcode-stdio-1.0\n    main-is:          Benchmarks.hs\n    hs-source-dirs:   Benchmarks Tests\n    other-modules:\n        Certificate\n        Connection\n        PipeChan\n        PubKey\n\n    default-language: Haskell2010\n    ghc-options:      -Wall -fno-warn-unused-imports\n    build-depends:\n        base >=4 && <5,\n        tls,\n        crypton-x509,\n        crypton-x509-validation,\n        data-default-class,\n        crypton,\n        gauge,\n        bytestring,\n        asn1-types,\n        async >=2.0,\n        hourglass,\n        QuickCheck >=2,\n        tasty-quickcheck,\n        tls\n";
    }