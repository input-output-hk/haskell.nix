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
      identifier = { name = "data-array-byte"; version = "0.1.0.1"; };
      license = "BSD-3-Clause";
      copyright = "(c) Roman Leshchinskiy 2009-2012";
      maintainer = "andrew.lelechenko@gmail.com";
      author = "Roman Leshchinskiy <rl@cse.unsw.edu.au>";
      homepage = "https://github.com/Bodigrim/data-array-byte";
      url = "";
      synopsis = "Compatibility layer for Data.Array.Byte";
      description = "Compatibility layer for [Data.Array.Byte](https://hackage.haskell.org/package/base/docs/Data-Array-Byte.html), providing boxed wrappers for @ByteArray#@ and @MutableByteArray#@ and relevant instances for GHC < 9.4. Include it into your Cabal file:\n\n> build-depends: base\n> if impl(ghc < 9.4)\n>   build-depends: data-array-byte\n\nand then @import Data.Array.Byte@ unconditionally.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          ];
        buildable = true;
        };
      tests = {
        "data-array-byte-tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."data-array-byte" or (errorHandler.buildDepError "data-array-byte"))
            (hsPkgs."quickcheck-classes-base" or (errorHandler.buildDepError "quickcheck-classes-base"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/data-array-byte-0.1.0.1.tar.gz";
      sha256 = "1bb6eca0b3e02d057fe7f4e14c81ef395216f421ab30fdaa1b18017c9c025600";
      });
    }) // {
    package-description-override = "cabal-version:      >=1.10\r\nname:               data-array-byte\r\nversion:            0.1.0.1\r\nx-revision: 1\r\nlicense:            BSD3\r\nlicense-file:       LICENSE\r\ncopyright:          (c) Roman Leshchinskiy 2009-2012\r\nmaintainer:         andrew.lelechenko@gmail.com\r\nauthor:             Roman Leshchinskiy <rl@cse.unsw.edu.au>\r\ntested-with:\r\n    ghc ==8.0.2 ghc ==8.2.2 ghc ==8.4.4 ghc ==8.6.5 ghc ==8.8.4\r\n    ghc ==8.10.7 ghc ==9.0.2 ghc ==9.2.4 ghc ==9.4.2\r\n\r\nhomepage:           https://github.com/Bodigrim/data-array-byte\r\nbug-reports:        https://github.com/Bodigrim/data-array-byte/issues\r\nsynopsis:           Compatibility layer for Data.Array.Byte\r\ndescription:\r\n    Compatibility layer for [Data.Array.Byte](https://hackage.haskell.org/package/base/docs/Data-Array-Byte.html), providing boxed wrappers for @ByteArray#@ and @MutableByteArray#@ and relevant instances for GHC < 9.4. Include it into your Cabal file:\r\n    .\r\n    > build-depends: base\r\n    > if impl(ghc < 9.4)\r\n    >   build-depends: data-array-byte\r\n    .\r\n    and then @import Data.Array.Byte@ unconditionally.\r\n\r\ncategory:           Compatibility\r\nbuild-type:         Simple\r\nextra-source-files:\r\n    changelog.md\r\n    README.md\r\n\r\nsource-repository head\r\n    type:     git\r\n    location: https://github.com/Bodigrim/data-array-byte\r\n\r\nlibrary\r\n    default-language: Haskell2010\r\n    ghc-options:      -Wall\r\n    build-depends:\r\n        base >=4.9 && <4.19,\r\n        deepseq >=1.4 && <1.5,\r\n        template-haskell >=2.11 && <2.21\r\n\r\n    if impl(ghc <9.4)\r\n        exposed-modules: Data.Array.Byte\r\n\r\ntest-suite data-array-byte-tests\r\n    type:             exitcode-stdio-1.0\r\n    main-is:          Main.hs\r\n    hs-source-dirs:   test\r\n    default-language: Haskell2010\r\n    ghc-options:      -Wall\r\n    build-depends:\r\n        base,\r\n        data-array-byte,\r\n        quickcheck-classes-base >=0.6 && <0.7,\r\n        tasty >=1.4 && <1.5,\r\n        tasty-quickcheck >=0.10 && <0.11,\r\n        template-haskell\r\n";
    }