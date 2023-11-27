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
    flags = {
      test-properties = true;
      test-hlint = true;
      test-doctests = true;
      no-donna = true;
      };
    package = {
      specVersion = "1.10";
      identifier = { name = "ed25519"; version = "0.0.5.0"; };
      license = "MIT";
      copyright = "Copyright (c) Austin Seipp 2013-2017";
      maintainer = "Austin Seipp <aseipp@pobox.com>";
      author = "Austin Seipp";
      homepage = "https://github.com/thoughtpolice/hs-ed25519/";
      url = "";
      synopsis = "Ed25519 cryptographic signatures";
      description = "This package provides a simple, fast, self-contained copy of the\nEd25519 public-key signature system with a clean interface. It also\nincludes support for detached signatures, and thorough documentation\non the design and implementation, including usage guidelines.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          ];
        buildable = if flags.no-donna then true else false;
        };
      tests = {
        "properties" = {
          depends = (pkgs.lib).optionals (!(!flags.test-properties)) [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."ed25519" or (errorHandler.buildDepError "ed25519"))
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
        "doctests" = {
          depends = (pkgs.lib).optionals (!(!flags.test-doctests)) [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."doctest" or (errorHandler.buildDepError "doctest"))
            ];
          buildable = if !flags.test-doctests then false else true;
          };
        };
      benchmarks = {
        "bench" = {
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
      url = "http://hackage.haskell.org/package/ed25519-0.0.5.0.tar.gz";
      sha256 = "d8a5958ebfa9309790efade64275dc5c441b568645c45ceed1b0c6ff36d6156d";
      });
    }) // {
    package-description-override = "cabal-version:       >=1.10\nname:                ed25519\nversion:             0.0.5.0\nx-revision:          8\ncategory:            Cryptography\nlicense:             MIT\nsynopsis:            Ed25519 cryptographic signatures\nhomepage:            https://github.com/thoughtpolice/hs-ed25519/\nbug-reports:         https://github.com/thoughtpolice/hs-ed25519/issues\nlicense-file:        LICENSE.txt\ncopyright:           Copyright (c) Austin Seipp 2013-2017\nauthor:              Austin Seipp\nmaintainer:          Austin Seipp <aseipp@pobox.com>\nbuild-type:          Simple\ntested-with:         GHC == 7.0.1, GHC == 7.0.2, GHC == 7.0.3, GHC == 7.0.4,\n                     GHC == 7.2.1, GHC == 7.2.2, GHC == 7.4.1, GHC == 7.4.2,\n                     GHC == 7.6.1, GHC == 7.6.2, GHC == 7.6.3,\n                     GHC == 7.8.2, GHC == 7.8.3, GHC == 7.8.4,\n                     GHC == 7.10.1, GHC == 7.10.2\n\ndescription:\n  This package provides a simple, fast, self-contained copy of the\n  Ed25519 public-key signature system with a clean interface. It also\n  includes support for detached signatures, and thorough documentation\n  on the design and implementation, including usage guidelines.\n\nextra-source-files:\n  .travis.yml\n  AUTHORS.txt\n  README.md\n  CONTRIBUTING.md\n  CHANGELOG.md\n  src/cbits/ref10/*.c\n  src/cbits/ref10/include/*.h\n\nsource-repository head\n  type: git\n  location: https://github.com/thoughtpolice/hs-ed25519.git\n\n-------------------------------------------------------------------------------\n-- Flags\n\nflag test-properties\n  default: True\n  manual: True\n\nflag test-hlint\n  default: True\n  manual: True\n\nflag test-doctests\n  default: True\n  manual: True\n\nflag no-donna\n  default: True\n  manual: True\n\n-------------------------------------------------------------------------------\n-- Build pt 1: main project\n\nlibrary\n  build-depends:\n    ghc-prim    >= 0.1 && < 0.12,\n    base        >= 4   && < 5,\n    bytestring  >= 0.9 && < 0.13\n\n  exposed-modules:\n    Crypto.Sign.Ed25519\n\n  ghc-options:        -Wall -fwarn-tabs\n  default-language:   Haskell2010\n  hs-source-dirs:     src\n\n  -- Choose the underlying C implementation\n  if flag(no-donna)\n    -- ref10 implementation from SUPERCOP, about 2x slower than the AMD64\n    -- SUPERCOP implementations, 15x faster than ronald3072 for signing.\n    c-sources:          src/cbits/ref10/ed25519.c\n    include-dirs:       src/cbits/ref10 src/cbits/ref10/include\n  else\n    -- TODO(aseipp): ed25519-donna import\n    buildable: False\n\n-------------------------------------------------------------------------------\n-- Build pt 2: Tests\n\ntest-suite properties\n  type: exitcode-stdio-1.0\n  main-is: properties.hs\n  ghc-options: -w\n  hs-source-dirs: tests\n  default-language:   Haskell2010\n\n  if !flag(test-properties)\n    buildable: False\n  else\n    build-depends:\n      base        >= 4   && < 5,\n      bytestring  >= 0.9 && < 0.12,\n      QuickCheck  >= 2.4 && < 2.9,\n      ed25519\n\n--\n-- Style/doc tests below\n--\n\ntest-suite hlint\n  type:             exitcode-stdio-1.0\n  main-is:          hlint.hs\n  hs-source-dirs:   tests\n  default-language: Haskell2010\n\n  if !flag(test-hlint)\n    buildable: False\n  else\n    build-depends:\n      base  >= 4   && < 5,\n      hlint >= 1.7 && < 1.10\n\ntest-suite doctests\n  type:             exitcode-stdio-1.0\n  main-is:          doctests.hs\n  hs-source-dirs:   tests\n  default-language: Haskell2010\n\n  if !flag(test-doctests)\n    buildable: False\n  else\n    build-depends:\n      base      >= 4    && < 5,\n      filepath  >= 1.0  && < 1.5,\n      directory >= 1.0  && < 1.3,\n      doctest   >= 0.10 && < 0.12\n\n-------------------------------------------------------------------------------\n-- Build pt 3: benchmarks\n\nbenchmark bench\n  type:               exitcode-stdio-1.0\n  build-depends:\n      base        >= 4   && < 5,\n      bytestring  >= 0.9 && < 0.12,\n      criterion   >= 0.8 && < 1.2,\n      deepseq     >= 1.3 && < 1.5,\n      ed25519\n\n  default-language:   Haskell2010\n  hs-source-dirs:     benchmarks\n  main-is:            bench.hs\n";
    }