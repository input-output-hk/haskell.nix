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
    flags = {};
    package = {
      specVersion = "1.8";
      identifier = { name = "cryptohash"; version = "0.11.9"; };
      license = "BSD-3-Clause";
      copyright = "Vincent Hanquez <vincent@snarc.org>";
      maintainer = "Vincent Hanquez <vincent@snarc.org>";
      author = "Vincent Hanquez <vincent@snarc.org>";
      homepage = "http://github.com/vincenthz/hs-cryptohash";
      url = "";
      synopsis = "collection of crypto hashes, fast, pure and practical";
      description = "DEPRECATED: this library is still fully functional, but please use cryptonite for new projects\nand convert old one to use cryptonite. This is where things are at nowadays.\n\nA collection of crypto hashes, with a practical incremental and one-pass, pure APIs,\nwith performance close to the fastest implementations available in other languages.\n\nThe implementations are made in C with a haskell FFI wrapper that hide the C implementation.\n\nSimple examples using the unified API:\n\n> import Crypto.Hash\n>\n> sha1 :: ByteString -> Digest SHA1\n> sha1 = hash\n>\n> hexSha3_512 :: ByteString -> String\n> hexSha3_512 bs = show (hash bs :: Digest SHA3_512)\n\nSimple examples using the module API:\n\n> import qualified Crypto.Hash.SHA1 as SHA1\n>\n> main = putStrLn $ show $ SHA1.hash (Data.ByteString.pack [0..255])\n\n> import qualified Crypto.Hash.SHA3 as SHA3\n>\n> main = putStrLn $ show $ digest\n>   where digest = SHA3.finalize ctx\n>         ctx    = foldl' SHA3.update iCtx (map Data.ByteString.pack [ [1,2,3], [4,5,6] ]\n>         iCtx   = SHA3.init 224";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."byteable" or (errorHandler.buildDepError "byteable"))
          (hsPkgs."cryptonite" or (errorHandler.buildDepError "cryptonite"))
          (hsPkgs."memory" or (errorHandler.buildDepError "memory"))
          (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
          ];
        buildable = true;
        };
      tests = {
        "test-kat" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."byteable" or (errorHandler.buildDepError "byteable"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."cryptohash" or (errorHandler.buildDepError "cryptohash"))
            ];
          buildable = true;
          };
        };
      benchmarks = {
        "bench-hashes" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."cryptohash" or (errorHandler.buildDepError "cryptohash"))
            ];
          buildable = true;
          };
        "bench-hmac" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."cryptohash" or (errorHandler.buildDepError "cryptohash"))
            (hsPkgs."byteable" or (errorHandler.buildDepError "byteable"))
            ];
          buildable = true;
          };
        "bench-api" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."cryptohash" or (errorHandler.buildDepError "cryptohash"))
            (hsPkgs."byteable" or (errorHandler.buildDepError "byteable"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/cryptohash-0.11.9.tar.gz";
      sha256 = "c28f847fc1fcd65b6eea2e74a100300af940919f04bb21d391f6a773968f22fb";
      });
    }