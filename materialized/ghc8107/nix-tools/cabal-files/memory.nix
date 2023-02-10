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
      support_bytestring = true;
      support_foundation = true;
      support_basement = true;
      support_deepseq = true;
      };
    package = {
      specVersion = "1.18";
      identifier = { name = "memory"; version = "0.16.0"; };
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
        depends = (((pkgs.lib).optionals (!(compiler.isGhc && (compiler.version).lt "8.0")) [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
          ] ++ (pkgs.lib).optional (flags.support_bytestring) (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))) ++ (pkgs.lib).optional (flags.support_deepseq) (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))) ++ (pkgs.lib).optional (flags.support_foundation || flags.support_basement) (hsPkgs."basement" or (errorHandler.buildDepError "basement"));
        buildable = if compiler.isGhc && (compiler.version).lt "8.0"
          then false
          else true;
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
      url = "http://hackage.haskell.org/package/memory-0.16.0.tar.gz";
      sha256 = "146da0f8b82bc59042193e9b6128916e3aefb0a301ab2f071449beb344f6fd7f";
      });
    }) // {
    package-description-override = "Name:                memory\r\nversion:             0.16.0\r\nx-revision: 1\r\nSynopsis:            memory and related abstraction stuff\r\nDescription:\r\n    Chunk of memory, polymorphic byte array management and manipulation\r\n    .\r\n    * A polymorphic byte array abstraction and function similar to strict ByteString.\r\n    .\r\n    * Different type of byte array abstraction.\r\n    .\r\n    * Raw memory IO operations (memory set, memory copy, ..)\r\n    .\r\n    * Aliasing with endianness support.\r\n    .\r\n    * Encoding : Base16, Base32, Base64.\r\n    .\r\n    * Hashing : FNV, SipHash\r\nLicense:             BSD3\r\nLicense-file:        LICENSE\r\nCopyright:           Vincent Hanquez <vincent@snarc.org>\r\nAuthor:              Vincent Hanquez <vincent@snarc.org>\r\nMaintainer:          vincent@snarc.org, Nicolas Di Prima <nicolas@primetype.co.uk>\r\nCategory:            memory\r\nStability:           experimental\r\nBuild-Type:          Simple\r\nHomepage:            https://github.com/vincenthz/hs-memory\r\nBug-Reports:         https://github.com/vincenthz/hs-memory/issues\r\ncabal-version:       1.18\r\nextra-doc-files:     README.md CHANGELOG.md\r\n\r\nsource-repository head\r\n  type: git\r\n  location: https://github.com/vincenthz/hs-memory\r\n\r\nFlag support_bytestring\r\n  Description:       add non-orphan bytearray support for bytestring\r\n  Default:           True\r\n  Manual:            True\r\n\r\nFlag support_foundation\r\n  Description:       add support for foundation strings and unboxed array (deprecated use support_basement)\r\n  Default:           True\r\n  Manual:            True\r\n\r\nFlag support_basement\r\n  Description:       add support for foundation strings and unboxed array\r\n  Default:           True\r\n  Manual:            True\r\n\r\nFlag support_deepseq\r\n  Description:       add deepseq instances for memory types\r\n  Default:           True\r\n  Manual:            True\r\n\r\nLibrary\r\n  Exposed-modules:   Data.ByteArray\r\n                     Data.ByteArray.Encoding\r\n                     Data.ByteArray.Mapping\r\n                     Data.ByteArray.Pack\r\n                     Data.ByteArray.Parse\r\n                     Data.ByteArray.Hash\r\n                     Data.Memory.Endian\r\n                     Data.Memory.PtrMethods\r\n                     Data.Memory.ExtendedWords\r\n                     Data.Memory.Encoding.Base16\r\n                     Data.Memory.Encoding.Base32\r\n                     Data.Memory.Encoding.Base64\r\n  Other-modules:     Data.Memory.Internal.Compat\r\n                     Data.Memory.Internal.CompatPrim\r\n                     Data.Memory.Internal.CompatPrim64\r\n                     Data.Memory.Internal.DeepSeq\r\n                     Data.Memory.Internal.Imports\r\n                     Data.Memory.Hash.SipHash\r\n                     Data.Memory.Hash.FNV\r\n                     Data.ByteArray.Pack.Internal\r\n                     Data.ByteArray.Types\r\n                     Data.ByteArray.Bytes\r\n                     Data.ByteArray.ScrubbedBytes\r\n                     Data.ByteArray.Methods\r\n                     Data.ByteArray.MemView\r\n                     Data.ByteArray.View\r\n  if impl(ghc < 8.0)\r\n    buildable: False\r\n  else\r\n    build-depends:   base < 4.16\r\n                   , ghc-prim < 0.8\r\n  -- FIXME armel or mispel is also little endian.\r\n  -- might be a good idea to also add a runtime autodetect mode.\r\n  -- ARCH_ENDIAN_UNKNOWN\r\n  if (arch(i386) || arch(x86_64))\r\n    CPP-options:     -DARCH_IS_LITTLE_ENDIAN\r\n  if os(windows)\r\n    Other-modules:   Data.Memory.MemMap.Windows\r\n  else\r\n    Other-modules:   Data.Memory.MemMap.Posix\r\n\r\n  -- optional support bytearray instance for bytestring\r\n  if flag(support_bytestring)\r\n    CPP-options:     -DWITH_BYTESTRING_SUPPORT\r\n    Build-depends:   bytestring\r\n  if flag(support_deepseq)\r\n    CPP-options:     -DWITH_DEEPSEQ_SUPPORT\r\n    Build-depends:   deepseq >= 1.1\r\n  if flag(support_foundation) || flag(support_basement)\r\n    CPP-options:     -DWITH_BASEMENT_SUPPORT\r\n    Build-depends:   basement >= 0.0.7\r\n    exposed-modules: Data.ByteArray.Sized\r\n\r\n  ghc-options:       -Wall -fwarn-tabs\r\n  default-language:  Haskell2010\r\n\r\nTest-Suite test-memory\r\n  type:              exitcode-stdio-1.0\r\n  hs-source-dirs:    tests\r\n  Main-is:           Tests.hs\r\n  Other-modules:     Imports\r\n                     SipHash\r\n                     Utils\r\n  if impl(ghc < 8.0)\r\n    buildable: False\r\n  else\r\n    build-depends:   base\r\n  Build-Depends:     bytestring\r\n                   , memory\r\n                   , basement >= 0.0.7\r\n                   , foundation\r\n  ghc-options:       -Wall -fno-warn-orphans -fno-warn-missing-signatures -threaded\r\n  default-language:  Haskell2010\r\n  if flag(support_foundation)\r\n    CPP-options:     -DWITH_BASEMENT_SUPPORT\r\n\r\n-- Test-Suite test-examples\r\n--   default-language:  Haskell2010\r\n--   type:              exitcode-stdio-1.0\r\n--   hs-source-dirs:    tests\r\n--   ghc-options:       -threaded\r\n--   Main-is:           DocTests.hs\r\n--   Build-Depends:     base >= 3 && < 5\r\n--                    , memory\r\n--                    , bytestring\r\n--                    , doctest\r\n";
    }