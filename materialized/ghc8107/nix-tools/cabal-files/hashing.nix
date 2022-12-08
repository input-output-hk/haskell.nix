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
      specVersion = "1.10";
      identifier = { name = "hashing"; version = "0.1.0.1"; };
      license = "MIT";
      copyright = "2016 Baojun Wang";
      maintainer = "wangbj@gmail.com";
      author = "Baojun Wang";
      homepage = "https://github.com/wangbj/hashing";
      url = "";
      synopsis = "A pure haskell library implements several hash algorithms.";
      description = "hashing is a pure haskell hash library, supported hash algorithm: SHA1\\/SHA224\\/SHA256\\/SHA384\\/SHA512\nMD5 and Whirlpool.\n\nIt provides a simple interface 'hash' and 'hashLazy' to compute a hash value Based on the type signature.\n\nPerformance wise it only has 1\\/5 - 1\\/15 of native C implementations, or 'cryptonite'.\n\nCompare to other pure hash libraries such as PureMD5/SHA, it provides a simpler interface,\nimplements more algorithms, and has less dependencies.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          ];
        buildable = true;
        };
      exes = {
        "hashing-exe" = {
          depends = [
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."array" or (errorHandler.buildDepError "array"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."hashing" or (errorHandler.buildDepError "hashing"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            ];
          buildable = true;
          };
        };
      tests = {
        "hashing-test" = {
          depends = [
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."array" or (errorHandler.buildDepError "array"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."hashing" or (errorHandler.buildDepError "hashing"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
            (hsPkgs."cryptonite" or (errorHandler.buildDepError "cryptonite"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/hashing-0.1.0.1.tar.gz";
      sha256 = "e5a4a19c6cd6f0a0adda381db76d608d23f8d303e68f1d744735433f91f49410";
      });
    }