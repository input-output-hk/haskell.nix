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
      identifier = { name = "hashing"; version = "0.1.1.0"; };
      license = "MIT";
      copyright = "2016 Baojun Wang";
      maintainer = "wangbj@gmail.com";
      author = "Baojun Wang";
      homepage = "https://github.com/wangbj/hashing";
      url = "";
      synopsis = "A pure haskell library implements several hash algorithms.";
      description = "hashing is a pure haskell hash library, supported hash algorithm: SHA1/SHA224/SHA256/SHA384/SHA512\nMD5 and Whirlpool. It provides a simple interface ``hash`` and ``hashLazy`` to compute a hash value\nBased on the type signature.\nPerformance wise it only has 1/5 - 1/15 of native C implementations, or ``cryptonite``.\nCompare to other pure hash libraries such as PureMD5/SHA, it provides a simpler interface,\nimplements more algorithms, and has less dependencies.";
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
      url = "http://hackage.haskell.org/package/hashing-0.1.1.0.tar.gz";
      sha256 = "71d9be6f44c1f786b7b28e09acdcb013e4f5ebe953e01bd9752fd54a9bd17ca8";
      });
    }) // {
    package-description-override = "name:                hashing\nversion:             0.1.1.0\nsynopsis:            A pure haskell library implements several hash algorithms.\ndescription:         hashing is a pure haskell hash library, supported hash algorithm: SHA1/SHA224/SHA256/SHA384/SHA512\n                     MD5 and Whirlpool. It provides a simple interface ``hash`` and ``hashLazy`` to compute a hash value\n                     Based on the type signature.\n                     Performance wise it only has 1/5 - 1/15 of native C implementations, or ``cryptonite``.\n                     Compare to other pure hash libraries such as PureMD5/SHA, it provides a simpler interface, \n                     implements more algorithms, and has less dependencies.\nhomepage:            https://github.com/wangbj/hashing\nlicense:             MIT\nlicense-file:        LICENSE\nauthor:              Baojun Wang\nmaintainer:          wangbj@gmail.com\ncopyright:           2016 Baojun Wang\ncategory:            Cryptography\nbuild-type:          Simple\n-- extra-source-files:\ncabal-version:       >=1.10\nbug-reports:         https://github.com/wangbj/hashing/issues\n\nlibrary\n  hs-source-dirs:      src\n  exposed-modules:     Crypto.Hash\n                     , Crypto.Hash.ADT\n                     , Crypto.Hash.SHA1\n                     , Crypto.Hash.SHA256\n                     , Crypto.Hash.SHA512\n                     , Crypto.Hash.MD5\n                     , Crypto.Hash.Whirlpool\n  build-depends:       array >= 0.5.1.0 && < 0.6\n                     , base >= 4.7 && < 5\n                     , bytestring >= 0.10.6.0 && < 0.12\n  default-language:    Haskell2010\n\nexecutable hashing-exe\n  hs-source-dirs:      app\n  main-is:             Main.hs\n  ghc-options:         -threaded -rtsopts -Wall -with-rtsopts=-N1\n  build-depends:       QuickCheck >= 2.8.1\n                     , array >= 0.5.1.0\n                     , base\n                     , bytestring >= 0.10.6.0\n                     , hashing\n                     , mtl >= 2.2.1\n  default-language:    Haskell2010\n\ntest-suite hashing-test\n  type:                exitcode-stdio-1.0\n  hs-source-dirs:      test\n  main-is:             Spec.hs\n  build-depends:       QuickCheck >= 2.8.1\n                     , array >= 0.5.1.0\n                     , base\n                     , bytestring >= 0.10.6.0\n                     , hashing >= 0.1.0.0\n                     , mtl >= 2.2.1\n                     , template-haskell >= 2.10.0.0\n                     , cryptonite >= 0.15\n  ghc-options:         -threaded -rtsopts -with-rtsopts=-N1\n  default-language:    Haskell2010\n\nsource-repository head\n  type:     git\n  location: https://github.com/wangbj/hashing\n";
    }