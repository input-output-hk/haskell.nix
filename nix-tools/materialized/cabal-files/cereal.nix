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
    flags = { bytestring-builder = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "cereal"; version = "0.5.8.3"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Eric Mertens <emertens@galois.com>";
      author = "Lennart Kolmodin <kolmodin@dtek.chalmers.se>,\nGalois Inc.,\nLemmih <lemmih@gmail.com>,\nBas van Dijk <v.dijk.bas@gmail.com>";
      homepage = "https://github.com/GaloisInc/cereal";
      url = "";
      synopsis = "A binary serialization library";
      description = "A binary serialization library, similar to binary, that introduces an isolate\nprimitive for parser isolation, and labeled blocks for better error messages.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = ([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
          ] ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8.0")) (hsPkgs."fail" or (errorHandler.buildDepError "fail"))) ++ (if flags.bytestring-builder
          then [
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."bytestring-builder" or (errorHandler.buildDepError "bytestring-builder"))
            ]
          else [
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            ]);
        buildable = true;
        };
      tests = {
        "test-cereal" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."test-framework" or (errorHandler.buildDepError "test-framework"))
            (hsPkgs."test-framework-quickcheck2" or (errorHandler.buildDepError "test-framework-quickcheck2"))
            (hsPkgs."cereal" or (errorHandler.buildDepError "cereal"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/cereal-0.5.8.3.tar.gz";
      sha256 = "99905220661b26e5bd91130bd9772554938608a5b1d717240a6eb331121e0f6a";
      });
    }) // {
    package-description-override = "name:                   cereal\nversion:                0.5.8.3\nlicense:                BSD3\nlicense-file:           LICENSE\nauthor:                 Lennart Kolmodin <kolmodin@dtek.chalmers.se>,\n                        Galois Inc.,\n                        Lemmih <lemmih@gmail.com>,\n                        Bas van Dijk <v.dijk.bas@gmail.com>\nmaintainer:             Eric Mertens <emertens@galois.com>\ncategory:               Data, Parsing\nstability:              provisional\nbuild-type:             Simple\ncabal-version:          >= 1.10\nsynopsis:               A binary serialization library\nhomepage:               https://github.com/GaloisInc/cereal\ntested-with:            GHC == 7.2.2, GHC == 7.4.2, GHC == 7.6.3, GHC == 7.8.4, GHC == 7.10.2\n\ndescription:\n  A binary serialization library, similar to binary, that introduces an isolate\n  primitive for parser isolation, and labeled blocks for better error messages.\n\nextra-source-files:     CHANGELOG.md\n\nsource-repository head\n  type:     git\n  location: git://github.com/GaloisInc/cereal.git\n\nflag bytestring-builder\n  description:\n    Decides whether to use an older version of bytestring along with bytestring-builder or just a newer version of bytestring.\n    .\n    This flag normally toggles automatically but you can use `-fbytestring-builder` or `-f-bytestring-builder` to explicitly change it.\n  default: False\n  manual: False\n\nlibrary\n        default-language:       Haskell2010\n\n        build-depends:          base >= 4.4 && < 5, containers, array,\n                                ghc-prim >= 0.2\n\n        if !impl(ghc >= 8.0)\n          build-depends:        fail == 4.9.*\n\n        if flag(bytestring-builder)\n          build-depends:        bytestring >= 0.9    && < 0.10.4,\n                                bytestring-builder >= 0.10.4 && < 1\n        else\n          build-depends:        bytestring >= 0.10.4 && < 1\n\n        hs-source-dirs:         src\n\n        exposed-modules:        Data.Serialize,\n                                Data.Serialize.Put,\n                                Data.Serialize.Get,\n                                Data.Serialize.IEEE754\n\n        ghc-options:            -Wall -O2 -funbox-strict-fields\n\n\n\ntest-suite test-cereal\n        default-language:       Haskell2010\n\n        type:                   exitcode-stdio-1.0\n\n        build-depends:          base == 4.*,\n                                bytestring >= 0.9,\n                                QuickCheck,\n                                test-framework,\n                                test-framework-quickcheck2,\n                                cereal\n\n        main-is:                Main.hs\n        other-modules:          RoundTrip\n                                GetTests\n\n        hs-source-dirs:         tests\n";
    }