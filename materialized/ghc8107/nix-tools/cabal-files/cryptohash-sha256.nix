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
    flags = { exe = false; use-cbits = true; };
    package = {
      specVersion = "2.0";
      identifier = { name = "cryptohash-sha256"; version = "0.11.102.1"; };
      license = "BSD-3-Clause";
      copyright = "Vincent Hanquez, Herbert Valerio Riedel";
      maintainer = "Herbert Valerio Riedel <hvr@gnu.org>";
      author = "";
      homepage = "https://github.com/hvr/cryptohash-sha256";
      url = "";
      synopsis = "Fast, pure and practical SHA-256 implementation";
      description = "A practical incremental and one-pass, pure API to\nthe [SHA-256 cryptographic hash algorithm](https://en.wikipedia.org/wiki/SHA-2) according\nto [FIPS 180-4](http://dx.doi.org/10.6028/NIST.FIPS.180-4)\nwith performance close to the fastest implementations available in other languages.\n\nThe core SHA-256 algorithm is implemented in C and is thus expected\nto be as fast as the standard [sha256sum(1) tool](https://linux.die.net/man/1/sha256sum);\nfor instance, on an /Intel Core i7-3770/ at 3.40GHz this implementation can\ncompute a SHA-256 hash over 230 MiB of data in under one second.\n(If, instead, you require a pure Haskell implementation and performance is secondary, please refer to the [SHA package](https://hackage.haskell.org/package/SHA).)\n\n\nAdditionally, this package provides support for\n\n- HMAC-SHA-256: SHA-256-based [Hashed Message Authentication Codes](https://en.wikipedia.org/wiki/HMAC) (HMAC)\n- HKDF-SHA-256: [HMAC-SHA-256-based Key Derivation Function](https://en.wikipedia.org/wiki/HKDF) (HKDF)\n\nconforming to [RFC6234](https://tools.ietf.org/html/rfc6234), [RFC4231](https://tools.ietf.org/html/rfc4231), [RFC5869](https://tools.ietf.org/html/rfc5869), et al..\n\n=== Relationship to the @cryptohash@ package and its API\n\nThis package has been originally a fork of @cryptohash-0.11.7@ because the @cryptohash@\npackage had been deprecated and so this package continues to satisfy the need for a\nlightweight package providing the SHA-256 hash algorithm without any dependencies on packages\nother than @base@ and @bytestring@. The API exposed by @cryptohash-sha256-0.11.*@'s\n\"Crypto.Hash.SHA256\" module is guaranteed to remain a compatible superset of the API provided\nby the @cryptohash-0.11.7@'s module of the same name.\n\nConsequently, this package is designed to be used as a drop-in replacement for @cryptohash-0.11.7@'s\n\"Crypto.Hash.SHA256\" module, though with\na [clearly smaller footprint by almost 3 orders of magnitude](https://www.reddit.com/r/haskell/comments/5lxv75/psa_please_use_unique_module_names_when_uploading/dbzegx3/).";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          ] ++ (if flags.use-cbits
          then [
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            ]
          else [
            (hsPkgs."cryptohash-sha256-pure" or (errorHandler.buildDepError "cryptohash-sha256-pure"))
            ]);
        buildable = true;
        };
      exes = {
        "sha256sum" = {
          depends = (pkgs.lib).optionals (flags.exe) [
            (hsPkgs."cryptohash-sha256" or (errorHandler.buildDepError "cryptohash-sha256"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
            ];
          buildable = if flags.exe then true else false;
          };
        };
      tests = {
        "test-sha256" = {
          depends = [
            (hsPkgs."cryptohash-sha256" or (errorHandler.buildDepError "cryptohash-sha256"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
            (hsPkgs."SHA" or (errorHandler.buildDepError "SHA"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            ];
          buildable = true;
          };
        };
      benchmarks = {
        "bench-sha256" = {
          depends = [
            (hsPkgs."cryptohash-sha256" or (errorHandler.buildDepError "cryptohash-sha256"))
            (hsPkgs."SHA" or (errorHandler.buildDepError "SHA"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/cryptohash-sha256-0.11.102.1.tar.gz";
      sha256 = "73a7dc7163871a80837495039a099967b11f5c4fe70a118277842f7a713c6bf6";
      });
    }