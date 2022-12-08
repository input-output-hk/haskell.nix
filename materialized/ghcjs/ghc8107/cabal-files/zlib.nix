{ system
  , compiler
  , flags
  , pkgs
  , hsPkgs
  , pkgconfPkgs
  , errorHandler
  , config
  , ... }:
  {
    flags = {
      non-blocking-ffi = false;
      pkg-config = false;
      bundled-c-zlib = false;
      };
    package = {
      specVersion = "1.10";
      identifier = { name = "zlib"; version = "0.6.2.3"; };
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
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/zlib-0.6.2.3.tar.gz";
      sha256 = "807f6bddf9cb3c517ce5757d991dde3c7e319953a22c86ee03d74534bd5abc88";
      });
    }