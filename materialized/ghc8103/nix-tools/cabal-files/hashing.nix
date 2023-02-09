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
    }) // {
    package-description-override = "name:                hashing\r\nversion:             0.1.0.1\r\nx-revision: 2\r\nsynopsis:            A pure haskell library implements several hash algorithms.\r\ndescription:         hashing is a pure haskell hash library, supported hash algorithm: SHA1\\/SHA224\\/SHA256\\/SHA384\\/SHA512\r\n                     MD5 and Whirlpool. \r\n                     .\r\n                     It provides a simple interface 'hash' and 'hashLazy' to compute a hash value Based on the type signature.\r\n                     .\r\n                     Performance wise it only has 1\\/5 - 1\\/15 of native C implementations, or 'cryptonite'.\r\n                     .\r\n                     Compare to other pure hash libraries such as PureMD5/SHA, it provides a simpler interface, \r\n                     implements more algorithms, and has less dependencies.\r\nhomepage:            https://github.com/wangbj/hashing\r\nlicense:             MIT\r\nlicense-file:        LICENSE\r\nauthor:              Baojun Wang\r\nmaintainer:          wangbj@gmail.com\r\ncopyright:           2016 Baojun Wang\r\ncategory:            Cryptography\r\nbuild-type:          Simple\r\n-- extra-source-files:\r\ncabal-version:       >=1.10\r\nbug-reports:         https://github.com/wangbj/hashing/issues\r\n\r\nlibrary\r\n  hs-source-dirs:      src\r\n  exposed-modules:     Crypto.Hash\r\n                     , Crypto.Hash.ADT\r\n                     , Crypto.Hash.SHA1\r\n                     , Crypto.Hash.SHA256\r\n                     , Crypto.Hash.SHA512\r\n                     , Crypto.Hash.MD5\r\n                     , Crypto.Hash.Whirlpool\r\n  build-depends:       array >= 0.5.1.0 && < 0.6\r\n                     , base >= 4.8 && < 5\r\n                     , bytestring >= 0.10.6.0 && < 0.11\r\n  default-language:    Haskell2010\r\n\r\nexecutable hashing-exe\r\n  hs-source-dirs:      app\r\n  main-is:             Main.hs\r\n  ghc-options:         -threaded -rtsopts -Wall -with-rtsopts=-N1\r\n  build-depends:       QuickCheck >= 2.8.1\r\n                     , array >= 0.5.1.0\r\n                     , base\r\n                     , bytestring >= 0.10.6.0\r\n                     , hashing\r\n                     , mtl >= 2.2.1\r\n  default-language:    Haskell2010\r\n\r\ntest-suite hashing-test\r\n  type:                exitcode-stdio-1.0\r\n  hs-source-dirs:      test\r\n  main-is:             Spec.hs\r\n  build-depends:       QuickCheck >= 2.8.1\r\n                     , array >= 0.5.1.0\r\n                     , base\r\n                     , bytestring >= 0.10.6.0\r\n                     , hashing >= 0.1.0.0\r\n                     , mtl >= 2.2.1\r\n                     , template-haskell >= 2.10.0.0\r\n                     , cryptonite >= 0.15\r\n  ghc-options:         -threaded -rtsopts -with-rtsopts=-N1\r\n  default-language:    Haskell2010\r\n\r\nsource-repository head\r\n  type:     git\r\n  location: https://github.com/wangbj/hashing\r\n";
    }