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
    flags = {
      support_aesni = true;
      support_rdrand = true;
      support_pclmuldq = false;
      support_sse = false;
      integer-gmp = true;
      support_deepseq = true;
      old_toolchain_inliner = false;
      check_alignment = false;
      use_target_attributes = true;
      };
    package = {
      specVersion = "1.18";
      identifier = { name = "cryptonite"; version = "0.28"; };
      license = "BSD-3-Clause";
      copyright = "Vincent Hanquez <vincent@snarc.org>";
      maintainer = "vincent@snarc.org";
      author = "Vincent Hanquez <vincent@snarc.org>";
      homepage = "https://github.com/haskell-crypto/cryptonite";
      url = "";
      synopsis = "Cryptography Primitives sink";
      description = "A repository of cryptographic primitives.\n\n* Symmetric ciphers: AES, DES, 3DES, CAST5, Blowfish, Twofish, Camellia, RC4, Salsa, XSalsa, ChaCha.\n\n* Hash: SHA1, SHA2, SHA3, SHAKE, MD2, MD4, MD5, Keccak, Skein, Ripemd, Tiger, Whirlpool, Blake2\n\n* MAC: HMAC, KMAC, Poly1305\n\n* Asymmetric crypto: DSA, RSA, DH, ECDH, ECDSA, ECC, Curve25519, Curve448, Ed25519, Ed448\n\n* Key Derivation Function: PBKDF2, Scrypt, HKDF, Argon2, BCrypt, BCryptPBKDF\n\n* Cryptographic Random generation: System Entropy, Deterministic Random Generator\n\n* Data related: Anti-Forensic Information Splitter (AFIS)\n\nIf anything cryptographic related is missing from here, submit\na pull request to have it added. This package strives to be a\ncryptographic kitchen sink that provides cryptography for everyone.\n\nEvaluate the security related to your requirements before using.\n\nRead \"Crypto.Tutorial\" for a quick start guide.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = ((([
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."memory" or (errorHandler.buildDepError "memory"))
          (hsPkgs."basement" or (errorHandler.buildDepError "basement"))
          (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
          ] ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).lt "8.0")) (hsPkgs."base" or (errorHandler.buildDepError "base"))) ++ (pkgs.lib).optional (system.isWindows) (hsPkgs."Win32" or (errorHandler.buildDepError "Win32"))) ++ (pkgs.lib).optional (compiler.isGhc && true && flags.integer-gmp) (hsPkgs."integer-gmp" or (errorHandler.buildDepError "integer-gmp"))) ++ (pkgs.lib).optional (flags.support_deepseq) (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"));
        libs = (pkgs.lib).optional (system.isLinux) (pkgs."pthread" or (errorHandler.sysDepError "pthread")) ++ (pkgs.lib).optional (system.isWindows) (pkgs."advapi32" or (errorHandler.sysDepError "advapi32"));
        buildable = if compiler.isGhc && (compiler.version).lt "8.0"
          then false
          else true;
        };
      tests = {
        "test-cryptonite" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."memory" or (errorHandler.buildDepError "memory"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-kat" or (errorHandler.buildDepError "tasty-kat"))
            (hsPkgs."cryptonite" or (errorHandler.buildDepError "cryptonite"))
            ];
          buildable = true;
          };
        };
      benchmarks = {
        "bench-cryptonite" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."memory" or (errorHandler.buildDepError "memory"))
            (hsPkgs."gauge" or (errorHandler.buildDepError "gauge"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."cryptonite" or (errorHandler.buildDepError "cryptonite"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/cryptonite-0.28.tar.gz";
      sha256 = "74ad886ae3f7cd6cadecb596707e49df37b0170ceed313e382bd15b13132a5db";
      });
    }