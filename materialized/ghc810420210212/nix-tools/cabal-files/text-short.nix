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
    flags = { asserts = false; };
    package = {
      specVersion = "1.18";
      identifier = { name = "text-short"; version = "0.1.5"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "hvr@gnu.org";
      author = "Herbert Valerio Riedel";
      homepage = "";
      url = "";
      synopsis = "Memory-efficient representation of Unicode text strings";
      description = "This package provides the 'ShortText' type which is suitable for keeping many short strings in memory. This is similiar to how 'ShortByteString' relates to 'ByteString'.\n\nThe main difference between 'Text' and 'ShortText' is that 'ShortText' uses UTF-8 instead of UTF-16 internally and also doesn't support zero-copy slicing (thereby saving 2 words). Consequently, the memory footprint of a (boxed) 'ShortText' value is 4 words (2 words when unboxed) plus the length of the UTF-8 encoded payload.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = ([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          ] ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).ge "8.0") (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))) ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8.0")) (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"));
        buildable = true;
        };
      tests = {
        "tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."text-short" or (errorHandler.buildDepError "text-short"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/text-short-0.1.5.tar.gz";
      sha256 = "a35ec6cde2ada084c1a050dc5885be5ab01f851b93d744cf0facbc1c18002dda";
      });
    }) // {
    package-description-override = "cabal-version:      1.18\nname:               text-short\nversion:            0.1.5\nx-revision:         1\nsynopsis:           Memory-efficient representation of Unicode text strings\nlicense:            BSD3\nlicense-file:       LICENSE\nauthor:             Herbert Valerio Riedel\nmaintainer:         hvr@gnu.org\nbug-reports:        https://github.com/hvr/text-short/issues\ncategory:           Data\nbuild-type:         Simple\ndescription:\n  This package provides the 'ShortText' type which is suitable for keeping many short strings in memory. This is similiar to how 'ShortByteString' relates to 'ByteString'.\n  .\n  The main difference between 'Text' and 'ShortText' is that 'ShortText' uses UTF-8 instead of UTF-16 internally and also doesn't support zero-copy slicing (thereby saving 2 words). Consequently, the memory footprint of a (boxed) 'ShortText' value is 4 words (2 words when unboxed) plus the length of the UTF-8 encoded payload.\n\ntested-with:\n  GHC ==7.8.4\n   || ==7.10.3\n   || ==8.0.2\n   || ==8.2.2\n   || ==8.4.4\n   || ==8.6.5\n   || ==8.8.3\n   || ==8.10.7\n   || ==9.0.2\n   || ==9.2.4\n   || ==9.4.2\n\nextra-source-files: ChangeLog.md\n\nsource-repository head\n  type:     git\n  location: https://github.com/hvr/text-short.git\n\nflag asserts\n  description: Enable runtime-checks via @assert@\n  default:     False\n  manual:      True\n\nlibrary\n  exposed-modules:\n    Data.Text.Short\n    Data.Text.Short.Partial\n    Data.Text.Short.Unsafe\n\n  other-modules:    Data.Text.Short.Internal\n  build-depends:\n      base              >=4.7     && <4.18\n    , binary            >=0.7.1   && <0.9\n    , bytestring        >=0.10.4  && <0.12\n    , deepseq           >=1.3     && <1.5\n    , ghc-prim          >=0.3.1   && <0.10\n    , hashable          >=1.2.6   && <1.5\n    , template-haskell  >=2.9.0.0 && <2.20\n    , text              >=1.0     && <1.3 || >=2.0 && <2.1\n\n  if impl(ghc >=8.0)\n    build-depends: bytestring >=0.10.8.0\n\n  if !impl(ghc >=8.0)\n    build-depends: semigroups >=0.18.2 && <0.21\n\n  -- GHC version specific PrimOps\n  if impl(ghc >=8.4)\n    hs-source-dirs: src-ghc804\n\n  else\n    c-sources:      cbits/memcmp.c\n    hs-source-dirs: src-ghc708\n\n  other-modules:    PrimOps\n  hs-source-dirs:   src\n  default-language: Haskell2010\n  other-extensions:\n    CPP\n    GeneralizedNewtypeDeriving\n    MagicHash\n    Trustworthy\n    UnliftedFFITypes\n    Unsafe\n\n  if impl(ghc >=8)\n    other-extensions: TemplateHaskellQuotes\n\n  else\n    other-extensions: TemplateHaskell\n\n  c-sources:        cbits/cbits.c\n\n  if flag(asserts)\n    ghc-options: -fno-ignore-asserts\n\n  else\n    cc-options: -DNDEBUG=1\n\n  ghc-options:      -Wall\n  cc-options:       -Wall\n\ntest-suite tests\n  type:             exitcode-stdio-1.0\n  hs-source-dirs:   src-test\n  main-is:          Tests.hs\n\n  -- bytestring dependency for cabal_macros.h\n  build-depends:\n      base\n    , binary\n    , bytestring\n    , template-haskell\n    , text\n    , text-short\n\n  -- deps which don't inherit constraints from library stanza:\n  build-depends:\n      tasty             >=1.4    && <1.5\n    , tasty-hunit       >=0.10.0 && <0.11\n    , tasty-quickcheck  >=0.10   && <0.11\n\n  default-language: Haskell2010\n";
    }