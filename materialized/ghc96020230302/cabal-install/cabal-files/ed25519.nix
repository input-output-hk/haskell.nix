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
    flags = { test-properties = true; test-hlint = true; };
    package = {
      specVersion = "1.10";
      identifier = { name = "ed25519"; version = "0.0.2.0"; };
      license = "MIT";
      copyright = "Copyright (c) Austin Seipp 2013";
      maintainer = "Austin Seipp <aseipp@pobox.com>";
      author = "Austin Seipp";
      homepage = "http://thoughtpolice.github.com/hs-ed25519";
      url = "";
      synopsis = "ed25519 cryptographic signatures";
      description = "This package provides a simple, portable implementation of the\ned25519 public-key signature system. It also includes support for\ndetached signatures.\n\nThe underlying implementation uses the @ref10@ implementation of\ned25519 from SUPERCOP, and should be relatively fast.\n\nFor more information (including how to get a copy of the software)\nvisit <http://ed25519.cr.yp.to>.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          ];
        buildable = true;
        };
      tests = {
        "properties" = {
          depends = (pkgs.lib).optionals (!(!flags.test-properties)) [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."ed25519" or (errorHandler.buildDepError "ed25519"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            ];
          buildable = if !flags.test-properties then false else true;
          };
        "hlint" = {
          depends = (pkgs.lib).optionals (!(!flags.test-hlint)) [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."hlint" or (errorHandler.buildDepError "hlint"))
            ];
          buildable = if !flags.test-hlint then false else true;
          };
        };
      benchmarks = {
        "bench1" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."ed25519" or (errorHandler.buildDepError "ed25519"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/ed25519-0.0.2.0.tar.gz";
      sha256 = "b2bac03694041e36ca82210a79c883b1813d8e2cfe49f4741feebc4092b80874";
      });
    }) // {
    package-description-override = "name:                ed25519\nversion:             0.0.2.0\ncategory:            Cryptography\nlicense:             MIT\nsynopsis:            ed25519 cryptographic signatures\nhomepage:            http://thoughtpolice.github.com/hs-ed25519\nbug-reports:         http://github.com/thoughtpolice/hs-ed25519/issues\nlicense-file:        LICENSE.txt\ncopyright:           Copyright (c) Austin Seipp 2013\nauthor:              Austin Seipp\nmaintainer:          Austin Seipp <aseipp@pobox.com>\nbuild-type:          Simple\ncabal-version:       >=1.10\ntested-with:         GHC == 6.12.3, GHC == 7.0.1, GHC == 7.0.2, GHC == 7.0.3,\n                     GHC == 7.0.4,  GHC == 7.2.1, GHC == 7.2.2, GHC == 7.4.1,\n                     GHC == 7.4.2,  GHC == 7.6.1,  GHC == 7.6.2, GHC == 7.6.3\n\ndescription:\n  This package provides a simple, portable implementation of the\n  ed25519 public-key signature system. It also includes support for\n  detached signatures.\n  .\n  The underlying implementation uses the @ref10@ implementation of\n  ed25519 from SUPERCOP, and should be relatively fast.\n  .\n  For more information (including how to get a copy of the software)\n  visit <http://ed25519.cr.yp.to>.\n\nextra-source-files:\n  .travis.yml\n  AUTHORS.txt\n  README.md\n  CONTRIBUTING.md\n  CHANGELOG.md\n  src/cbits/*.c\n  src/cbits/include/*.h\n\nsource-repository head\n  type: git\n  location: https://github.com/thoughtpolice/hs-ed25519.git\n\n-------------------------------------------------------------------------------\n-- Flags\n\nflag test-properties\n  default: True\n  manual: True\n\nflag test-hlint\n  default: True\n  manual: True\n\n-------------------------------------------------------------------------------\n-- Build pt 1: main project\n\nlibrary\n  build-depends:\n    base >= 4 && < 5,\n    bytestring\n\n  exposed-modules:\n    Crypto.Sign.Ed25519\n\n  ghc-options:        -Wall -fwarn-tabs\n  default-language:   Haskell98\n  hs-source-dirs:     src\n  c-sources:          src/cbits/ed25519.c\n  include-dirs:       src/cbits src/cbits/include\n\n-------------------------------------------------------------------------------\n-- Build pt 2: Tests\n\ntest-suite properties\n  type: exitcode-stdio-1.0\n  main-is: properties.hs\n  ghc-options: -w\n  hs-source-dirs: tests\n  default-language:   Haskell98\n\n  if !flag(test-properties)\n    buildable: False\n  else\n    build-depends:\n      base,\n      ed25519,\n      bytestring,\n      QuickCheck  >= 2.4\n\n--\n-- Style/doc tests below\n--\n\ntest-suite hlint\n  type: exitcode-stdio-1.0\n  main-is: hlint.hs\n  ghc-options: -w\n  hs-source-dirs: tests\n  default-language:   Haskell98\n\n  if !flag(test-hlint)\n    buildable: False\n  else\n    build-depends:\n      base,\n      hlint >= 1.7\n\n-------------------------------------------------------------------------------\n-- Build pt 3: benchmarks\n\nbenchmark bench1\n  type:               exitcode-stdio-1.0\n  build-depends:\n    base >= 4 && < 5,\n    bytestring,\n    criterion,\n    deepseq,\n    ed25519\n\n  default-language:   Haskell98\n  hs-source-dirs:     benchmarks\n  main-is:            bench1.hs\n";
    }