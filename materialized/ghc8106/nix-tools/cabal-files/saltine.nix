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
      specVersion = "2.0";
      identifier = { name = "saltine"; version = "0.2.0.1"; };
      license = "MIT";
      copyright = "Copyright (c) Joseph Abrahamson 2013";
      maintainer = "Max Amanshauser <max@lambdalifting.org>";
      author = "Joseph Abrahamson";
      homepage = "";
      url = "";
      synopsis = "Cryptography that's easy to digest (NaCl/libsodium bindings).";
      description = "/NaCl/ (pronounced \\\"salt\\\") is a new easy-to-use high-speed software\nlibrary for network communication, encryption, decryption,\nsignatures, etc. NaCl's goal is to provide all of the core\noperations needed to build higher-level cryptographic tools.\n\n<http://nacl.cr.yp.to/>\n\n/Sodium/ is a portable, cross-compilable, installable, packageable\ncrypto library based on NaCl, with a compatible API.\n\n<https://github.com/jedisct1/libsodium>\n\n/Saltine/ is a Haskell binding to the NaCl primitives going through\nSodium for build convenience and, eventually, portability.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."profunctors" or (errorHandler.buildDepError "profunctors"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          ];
        libs = (pkgs.lib).optional (system.isWindows) (pkgs."sodium" or (errorHandler.sysDepError "sodium"));
        pkgconfig = (pkgs.lib).optional (!system.isWindows) (pkgconfPkgs."libsodium" or (errorHandler.pkgConfDepError "libsodium"));
        buildable = true;
        };
      tests = {
        "tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."saltine" or (errorHandler.buildDepError "saltine"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."test-framework-quickcheck2" or (errorHandler.buildDepError "test-framework-quickcheck2"))
            (hsPkgs."test-framework" or (errorHandler.buildDepError "test-framework"))
            (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
            ];
          buildable = true;
          };
        };
      benchmarks = {
        "benchmarks" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."saltine" or (errorHandler.buildDepError "saltine"))
            ];
          libs = [ (pkgs."sodium" or (errorHandler.sysDepError "sodium")) ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/saltine-0.2.0.1.tar.gz";
      sha256 = "ce6867a08a308bc5155cb4c5a146837cf6c5e056dd0d0151f9f0838b2eed2ea0";
      });
    }) // {
    package-description-override = "cabal-version: 2.0\n\nname:                saltine\nversion:             0.2.0.1\nsynopsis:            Cryptography that's easy to digest (NaCl/libsodium bindings).\ndescription:\n\n  /NaCl/ (pronounced \\\"salt\\\") is a new easy-to-use high-speed software\n  library for network communication, encryption, decryption,\n  signatures, etc. NaCl's goal is to provide all of the core\n  operations needed to build higher-level cryptographic tools.\n  .\n  <http://nacl.cr.yp.to/>\n  .\n  /Sodium/ is a portable, cross-compilable, installable, packageable\n  crypto library based on NaCl, with a compatible API.\n  .\n  <https://github.com/jedisct1/libsodium>\n  .\n  /Saltine/ is a Haskell binding to the NaCl primitives going through\n  Sodium for build convenience and, eventually, portability.\n\nextra-source-files:\n                     README.md\n                     CHANGELOG.md\n\nlicense:             MIT\nlicense-file:        LICENSE\nauthor:              Joseph Abrahamson\nmaintainer:          Max Amanshauser <max@lambdalifting.org>\nbug-reports:         http://github.com/tel/saltine/issues\ncopyright:           Copyright (c) Joseph Abrahamson 2013\ncategory:            Cryptography\nbuild-type:          Simple\ntested-with:         GHC==8.0.2, GHC==8.2.2, GHC==8.4.4, GHC==8.6.5, GHC==8.8.4, GHC==8.10.7, GHC==9.0.2, GHC==9.2.2\n\nsource-repository head\n  type: git\n  location: https://github.com/tel/saltine.git\n\nlibrary\n  hs-source-dirs:     src\n  exposed-modules:\n                  Crypto.Saltine\n                  Crypto.Saltine.Class\n                  Crypto.Saltine.Core.SecretBox\n                  Crypto.Saltine.Core.AEAD\n                  Crypto.Saltine.Core.AEAD.AES256GCM\n                  Crypto.Saltine.Core.AEAD.ChaCha20Poly1305\n                  Crypto.Saltine.Core.AEAD.ChaCha20Poly1305IETF\n                  Crypto.Saltine.Core.AEAD.XChaCha20Poly1305\n                  Crypto.Saltine.Core.Box\n                  Crypto.Saltine.Core.Stream\n                  Crypto.Saltine.Core.Auth\n                  Crypto.Saltine.Core.OneTimeAuth\n                  Crypto.Saltine.Core.Sign\n                  Crypto.Saltine.Core.Hash\n                  Crypto.Saltine.Core.ScalarMult\n                  Crypto.Saltine.Core.Password\n                  Crypto.Saltine.Core.Utils\n                  Crypto.Saltine.Internal.AEAD.AES256GCM\n                  Crypto.Saltine.Internal.AEAD.ChaCha20Poly1305\n                  Crypto.Saltine.Internal.AEAD.ChaCha20Poly1305IETF\n                  Crypto.Saltine.Internal.AEAD.XChaCha20Poly1305\n                  Crypto.Saltine.Internal.Auth\n                  Crypto.Saltine.Internal.Box\n                  Crypto.Saltine.Internal.ByteSizes\n                  Crypto.Saltine.Internal.Hash\n                  Crypto.Saltine.Internal.OneTimeAuth\n                  Crypto.Saltine.Internal.Password\n                  Crypto.Saltine.Internal.ScalarMult\n                  Crypto.Saltine.Internal.SecretBox\n                  Crypto.Saltine.Internal.Sign\n                  Crypto.Saltine.Internal.Stream\n                  Crypto.Saltine.Internal.Util\n  other-modules:\n\n  if os(windows)\n    extra-libraries: sodium\n  else\n    pkgconfig-depends: libsodium >= 1.0.18\n\n  cc-options:         -Wall\n  ghc-options:        -Wall -funbox-strict-fields\n  default-language:   Haskell2010\n  build-depends:\n                base        >= 4.5    && < 5\n              , bytestring  >= 0.10.8 && < 0.12\n              , deepseq    ^>= 1.4\n              , profunctors >= 5.3    && < 5.7\n              , hashable\n              , text       ^>= 1.2 || ^>= 2.0\n\ntest-suite tests\n  type:    exitcode-stdio-1.0\n  main-is: Main.hs\n  other-modules:\n                AuthProperties\n                BoxProperties\n                HashProperties\n                OneTimeAuthProperties\n                PasswordProperties\n                ScalarMultProperties\n                SecretBoxProperties\n                SealedBoxProperties\n                SignProperties\n                StreamProperties\n                AEAD.AES256GCMProperties\n                AEAD.ChaCha20Poly1305IETFProperties\n                AEAD.ChaCha20Poly1305Properties\n                AEAD.XChaCha20Poly1305Properties\n                Util\n                UtilProperties\n  ghc-options: -Wall -threaded -rtsopts\n  hs-source-dirs: tests\n  default-language: Haskell2010\n  build-depends:\n                base >= 4.7 && < 5\n              , saltine\n              , bytestring\n              , text\n              , QuickCheck\n              , test-framework-quickcheck2\n              , test-framework\n              , semigroups\n\nbenchmark benchmarks\n  type: exitcode-stdio-1.0\n  main-is: Main.hs\n  hs-source-dirs:\n      bench\n  ghc-options: -rtsopts -threaded -with-rtsopts=-N -O2\n  extra-libraries:\n      sodium\n  build-depends:\n      base\n    , bytestring\n    , text\n    , criterion\n    , deepseq\n    , saltine\n  other-modules:\n      AuthBench\n      OneTimeAuthBench\n      ConstantTimeBench\n      BoxBench\n      SecretBoxBench\n      HashBench\n      RandomBench\n      PasswordBench\n      ScalarMultBench\n      SignBench\n      StreamBench\n      BenchUtils\n      AES256GCMBench\n      ChaCha20Poly1305Bench\n      ChaCha20Poly1305IETFBench\n      XChaCha20Poly1305Bench\n  default-language: Haskell2010\n";
    }