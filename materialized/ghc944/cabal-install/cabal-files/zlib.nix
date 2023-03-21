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
      non-blocking-ffi = false;
      pkg-config = false;
      bundled-c-zlib = false;
      };
    package = {
      specVersion = "1.10";
      identifier = { name = "zlib"; version = "0.6.3.0"; };
      license = "BSD-3-Clause";
      copyright = "(c) 2006-2016 Duncan Coutts";
      maintainer = "Duncan Coutts <duncan@community.haskell.org>, Andrew Lelechenko <andrew.lelechenko@gmail.com>, Emily Pillmore <emilypi@cohomolo.gy>, Herbert Valerio Riedel <hvr@gnu.org>";
      author = "Duncan Coutts <duncan@community.haskell.org>";
      homepage = "";
      url = "";
      synopsis = "Compression and decompression in the gzip and zlib formats";
      description = "This package provides a pure interface for compressing and\ndecompressing streams of data represented as lazy\n'ByteString's. It uses the\n<https://en.wikipedia.org/wiki/Zlib zlib C library>\nso it has high performance. It supports the \\\"zlib\\\",\n\\\"gzip\\\" and \\\"raw\\\" compression formats.\n\nIt provides a convenient high level API suitable for most\ntasks and for the few cases where more control is needed it\nprovides access to the full zlib feature set.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          ] ++ (pkgs.lib).optional (compiler.isGhc && ((compiler.version).ge "7.0" && (compiler.version).lt "8.0.3")) (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"));
        libs = (pkgs.lib).optionals (!(flags.pkg-config && !(compiler.isGhcjs && true) && !system.isGhcjs)) ((pkgs.lib).optional (!system.isWindows && !flags.bundled-c-zlib && !(compiler.isGhcjs && true) && !system.isGhcjs) (pkgs."z" or (errorHandler.sysDepError "z")));
        pkgconfig = (pkgs.lib).optional (flags.pkg-config && !(compiler.isGhcjs && true) && !system.isGhcjs) (pkgconfPkgs."zlib" or (errorHandler.pkgConfDepError "zlib"));
        buildable = true;
        };
      tests = {
        "tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."zlib" or (errorHandler.buildDepError "zlib"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/zlib-0.6.3.0.tar.gz";
      sha256 = "9eaa989ad4534438b5beb51c1d3a4c8f6a088fdff0b259a5394fbf39aaee04da";
      });
    }) // {
    package-description-override = "cabal-version:   >= 1.10\r\nname:            zlib\r\nversion:         0.6.3.0\r\nx-revision: 1\r\n\r\ncopyright:       (c) 2006-2016 Duncan Coutts\r\nlicense:         BSD3\r\nlicense-file:    LICENSE\r\nauthor:          Duncan Coutts <duncan@community.haskell.org>\r\nmaintainer:      Duncan Coutts <duncan@community.haskell.org>, Andrew Lelechenko <andrew.lelechenko@gmail.com>, Emily Pillmore <emilypi@cohomolo.gy>, Herbert Valerio Riedel <hvr@gnu.org>\r\nbug-reports:     https://github.com/haskell/zlib/issues\r\ncategory:        Codec\r\nsynopsis:        Compression and decompression in the gzip and zlib formats\r\ndescription:     This package provides a pure interface for compressing and\r\n                 decompressing streams of data represented as lazy\r\n                 'ByteString's. It uses the\r\n                 <https://en.wikipedia.org/wiki/Zlib zlib C library>\r\n                 so it has high performance. It supports the \\\"zlib\\\",\r\n                 \\\"gzip\\\" and \\\"raw\\\" compression formats.\r\n                 .\r\n                 It provides a convenient high level API suitable for most\r\n                 tasks and for the few cases where more control is needed it\r\n                 provides access to the full zlib feature set.\r\nbuild-type:      Simple\r\n\r\ntested-with:     GHC == 7.0.4\r\n               , GHC == 7.2.2\r\n               , GHC == 7.4.2\r\n               , GHC == 7.6.3\r\n               , GHC == 7.8.4\r\n               , GHC == 7.10.3\r\n               , GHC == 8.0.2\r\n               , GHC == 8.2.2\r\n               , GHC == 8.4.4\r\n               , GHC == 8.6.5\r\n               , GHC == 8.8.4\r\n               , GHC == 8.10.7\r\n               , GHC == 9.0.2\r\n               , GHC == 9.2.2\r\n\r\nextra-source-files: changelog\r\n                    README.md\r\n                    -- zlib C sources (for Windows)\r\n                    cbits/crc32.h cbits/inffast.h cbits/inflate.h\r\n                    cbits/trees.h cbits/deflate.h cbits/inffixed.h\r\n                    cbits/inftrees.h cbits/zutil.h cbits/gzguts.h\r\n                    -- test data files\r\n                    test/data/bad-crc.gz test/data/custom-dict.zlib\r\n                    test/data/custom-dict.zlib-dict test/data/hello.gz\r\n                    test/data/not-gzip test/data/two-files.gz\r\n                    -- demo programs:\r\n                    examples/gzip.hs examples/gunzip.hs\r\n\r\nsource-repository head\r\n  type: git\r\n  location: https://github.com/haskell/zlib.git\r\n\r\nflag non-blocking-ffi\r\n  default:     False\r\n  manual:      True\r\n  description: The (de)compression calls can sometimes take a long time, which\r\n               prevents other Haskell threads running. Enabling this flag\r\n               avoids this unfairness, but with greater overall cost.\r\n\r\nflag pkg-config\r\n  default:     False\r\n  manual:      True\r\n  description: Use @pkg-config(1)@ to locate foreign @zlib@ library.\r\n\r\nflag bundled-c-zlib\r\n  default:     False\r\n  manual:      True\r\n  description: Use the bundled zlib C sources. Requires pkg-config to be False.\r\n               For windows, this is the default.\r\n\r\n\r\nlibrary\r\n  exposed-modules: Codec.Compression.GZip,\r\n                   Codec.Compression.Zlib,\r\n                   Codec.Compression.Zlib.Raw,\r\n                   Codec.Compression.Zlib.Internal\r\n  other-modules:   Codec.Compression.Zlib.Stream,\r\n                   Codec.Compression.Zlib.ByteStringCompat\r\n\r\n  if impl(ghc < 7)\r\n    default-language: Haskell98\r\n    default-extensions: PatternGuards\r\n  else\r\n    default-language: Haskell2010\r\n\r\n  other-extensions: CPP, ForeignFunctionInterface, RankNTypes, BangPatterns,\r\n                    DeriveDataTypeable\r\n  if impl(ghc >= 7.2)\r\n    other-extensions: DeriveGeneric\r\n  if impl(ghc >= 7.6)\r\n    other-extensions: CApiFFI\r\n\r\n  build-depends:   base >= 4 && < 4.19,\r\n                   bytestring >= 0.9 && < 0.12\r\n  if impl(ghc >= 7.0 && < 8.0.3)\r\n    build-depends: ghc-prim\r\n\r\n  includes:        zlib.h\r\n  ghc-options:     -Wall -fwarn-tabs\r\n  if flag(non-blocking-ffi)\r\n    cpp-options:   -DNON_BLOCKING_FFI\r\n  if flag(pkg-config) && !impl(ghcjs) && !os(ghcjs)\r\n    -- NB: pkg-config is available on windows as well when using msys2\r\n    pkgconfig-depends: zlib\r\n  else\r\n    -- don't use pkg-config\r\n    if !os(windows) && !flag(bundled-c-zlib) && !impl(ghcjs) && !os(ghcjs)\r\n      -- Normally we use the the standard system zlib.\r\n      extra-libraries: z\r\n    else\r\n      -- However for the benefit of users of Windows (which does not have zlib\r\n      -- by default) we bundle a complete copy of the C sources of zlib-1.2.11\r\n      c-sources:   cbits/adler32.c cbits/compress.c cbits/crc32.c\r\n                   cbits/deflate.c cbits/infback.c\r\n                   cbits/inffast.c cbits/inflate.c cbits/inftrees.c\r\n                   cbits/trees.c cbits/uncompr.c cbits/zutil.c\r\n      include-dirs:  cbits\r\n      install-includes: zlib.h zconf.h\r\n\r\ntest-suite tests\r\n  type: exitcode-stdio-1.0\r\n  main-is:         Test.hs\r\n  other-modules:   Utils,\r\n                   Test.Codec.Compression.Zlib.Internal,\r\n                   Test.Codec.Compression.Zlib.Stream\r\n  hs-source-dirs:  test\r\n  default-language: Haskell2010\r\n  build-depends:   base, bytestring, zlib,\r\n                   QuickCheck       == 2.*,\r\n                   tasty            >= 0.8 && < 1.5,\r\n                   tasty-quickcheck >= 0.8 && < 0.11\r\n  ghc-options:     -Wall\r\n";
    }