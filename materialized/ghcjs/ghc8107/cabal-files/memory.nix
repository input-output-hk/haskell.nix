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
      support_bytestring = true;
      support_foundation = true;
      support_basement = true;
      support_deepseq = true;
      };
    package = {
      specVersion = "1.18";
      identifier = { name = "memory"; version = "0.15.0"; };
      license = "BSD-3-Clause";
      copyright = "Vincent Hanquez <vincent@snarc.org>";
      maintainer = "vincent@snarc.org, Nicolas Di Prima <nicolas@primetype.co.uk>";
      author = "Vincent Hanquez <vincent@snarc.org>";
      homepage = "https://github.com/vincenthz/hs-memory";
      url = "";
      synopsis = "memory and related abstraction stuff";
      description = "Chunk of memory, polymorphic byte array management and manipulation\n\n* A polymorphic byte array abstraction and function similar to strict ByteString.\n\n* Different type of byte array abstraction.\n\n* Raw memory IO operations (memory set, memory copy, ..)\n\n* Aliasing with endianness support.\n\n* Encoding : Base16, Base32, Base64.\n\n* Hashing : FNV, SipHash";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = ((([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          ] ++ (if compiler.isGhc && (compiler.version).lt "8.0"
          then [ (hsPkgs."base" or (errorHandler.buildDepError "base")) ]
          else [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
            ])) ++ (pkgs.lib).optional (flags.support_bytestring) (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))) ++ (pkgs.lib).optional (flags.support_deepseq) (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))) ++ (pkgs.lib).optional (flags.support_foundation || flags.support_basement) (hsPkgs."basement" or (errorHandler.buildDepError "basement"));
        buildable = true;
        };
      tests = {
        "test-memory" = {
          depends = [
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."memory" or (errorHandler.buildDepError "memory"))
            (hsPkgs."basement" or (errorHandler.buildDepError "basement"))
            (hsPkgs."foundation" or (errorHandler.buildDepError "foundation"))
            ] ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).lt "8.0")) (hsPkgs."base" or (errorHandler.buildDepError "base"));
          buildable = if compiler.isGhc && (compiler.version).lt "8.0"
            then false
            else true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/memory-0.15.0.tar.gz";
      sha256 = "e3ff892c1a94708954d0bb2c4f4ab81bc0f505352d95095319c462db1aeb3529";
      });
    }