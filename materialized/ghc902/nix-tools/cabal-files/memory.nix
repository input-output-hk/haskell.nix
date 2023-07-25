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
    flags = { support_bytestring = true; support_deepseq = true; };
    package = {
      specVersion = "1.18";
      identifier = { name = "memory"; version = "0.18.0"; };
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
        depends = (([
          (hsPkgs."basement" or (errorHandler.buildDepError "basement"))
          ] ++ (pkgs.lib).optionals (!(compiler.isGhc && (compiler.version).lt "8.8")) [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
          ]) ++ (pkgs.lib).optional (flags.support_bytestring) (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))) ++ (pkgs.lib).optional (flags.support_deepseq) (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"));
        buildable = if compiler.isGhc && (compiler.version).lt "8.8"
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
            ] ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).lt "8.8")) (hsPkgs."base" or (errorHandler.buildDepError "base"));
          buildable = if compiler.isGhc && (compiler.version).lt "8.8"
            then false
            else true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/memory-0.18.0.tar.gz";
      sha256 = "fd4eb6f638e24b81b4e6cdd68772a531726f2f67686c8969d3407d82f7862e3e";
      });
    }) // {
    package-description-override = "Name:                memory\nversion:             0.18.0\nSynopsis:            memory and related abstraction stuff\nDescription:\n    Chunk of memory, polymorphic byte array management and manipulation\n    .\n    * A polymorphic byte array abstraction and function similar to strict ByteString.\n    .\n    * Different type of byte array abstraction.\n    .\n    * Raw memory IO operations (memory set, memory copy, ..)\n    .\n    * Aliasing with endianness support.\n    .\n    * Encoding : Base16, Base32, Base64.\n    .\n    * Hashing : FNV, SipHash\nLicense:             BSD3\nLicense-file:        LICENSE\nCopyright:           Vincent Hanquez <vincent@snarc.org>\nAuthor:              Vincent Hanquez <vincent@snarc.org>\nMaintainer:          vincent@snarc.org, Nicolas Di Prima <nicolas@primetype.co.uk>\nCategory:            memory\nStability:           experimental\nBuild-Type:          Simple\nHomepage:            https://github.com/vincenthz/hs-memory\nBug-Reports:         https://github.com/vincenthz/hs-memory/issues\ncabal-version:       1.18\nextra-doc-files:     README.md CHANGELOG.md\n\nsource-repository head\n  type: git\n  location: https://github.com/vincenthz/hs-memory\n\nFlag support_bytestring\n  Description:       add non-orphan bytearray support for bytestring\n  Default:           True\n  Manual:            True\n\nFlag support_deepseq\n  Description:       add deepseq instances for memory types\n  Default:           True\n  Manual:            True\n\nLibrary\n  Exposed-modules:   Data.ByteArray\n                     Data.ByteArray.Encoding\n                     Data.ByteArray.Mapping\n                     Data.ByteArray.Pack\n                     Data.ByteArray.Parse\n                     Data.ByteArray.Hash\n                     Data.Memory.Endian\n                     Data.Memory.PtrMethods\n                     Data.Memory.ExtendedWords\n                     Data.Memory.Encoding.Base16\n                     Data.Memory.Encoding.Base32\n                     Data.Memory.Encoding.Base64\n  Other-modules:     Data.Memory.Internal.Compat\n                     Data.Memory.Internal.CompatPrim\n                     Data.Memory.Internal.CompatPrim64\n                     Data.Memory.Internal.DeepSeq\n                     Data.Memory.Internal.Imports\n                     Data.Memory.Hash.SipHash\n                     Data.Memory.Hash.FNV\n                     Data.ByteArray.Pack.Internal\n                     Data.ByteArray.Types\n                     Data.ByteArray.Bytes\n                     Data.ByteArray.ScrubbedBytes\n                     Data.ByteArray.Methods\n                     Data.ByteArray.MemView\n                     Data.ByteArray.View\n  if impl(ghc < 8.8)\n    buildable: False\n  else\n    build-depends:   base\n                   , ghc-prim\n  -- FIXME armel or mispel is also little endian.\n  -- might be a good idea to also add a runtime autodetect mode.\n  -- ARCH_ENDIAN_UNKNOWN\n  if (arch(i386) || arch(x86_64))\n    CPP-options:     -DARCH_IS_LITTLE_ENDIAN\n  if os(windows)\n    Other-modules:   Data.Memory.MemMap.Windows\n  else\n    Other-modules:   Data.Memory.MemMap.Posix\n\n  -- optional support bytearray instance for bytestring\n  if flag(support_bytestring)\n    CPP-options:     -DWITH_BYTESTRING_SUPPORT\n    Build-depends:   bytestring\n  if flag(support_deepseq)\n    CPP-options:     -DWITH_DEEPSEQ_SUPPORT\n    Build-depends:   deepseq >= 1.1\n\n  CPP-options:     -DWITH_BASEMENT_SUPPORT\n  Build-depends:   basement >= 0.0.7\n  exposed-modules: Data.ByteArray.Sized\n\n  ghc-options:       -Wall -fwarn-tabs\n  default-language:  Haskell2010\n\nTest-Suite test-memory\n  type:              exitcode-stdio-1.0\n  hs-source-dirs:    tests\n  Main-is:           Tests.hs\n  Other-modules:     Imports\n                     SipHash\n                     Utils\n  if impl(ghc < 8.8)\n    buildable: False\n  else\n    build-depends:   base\n  Build-Depends:     bytestring\n                   , memory\n                   , basement >= 0.0.7\n                   , foundation\n  ghc-options:       -Wall -fno-warn-orphans -fno-warn-missing-signatures -threaded\n  default-language:  Haskell2010\n  CPP-options:     -DWITH_BASEMENT_SUPPORT\n\n-- Test-Suite test-examples\n--   default-language:  Haskell2010\n--   type:              exitcode-stdio-1.0\n--   hs-source-dirs:    tests\n--   ghc-options:       -threaded\n--   Main-is:           DocTests.hs\n--   Build-Depends:     base >= 3 && < 5\n--                    , memory\n--                    , bytestring\n--                    , doctest\n";
    }