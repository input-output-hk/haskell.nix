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
    flags = {};
    package = {
      specVersion = "1.12";
      identifier = { name = "base16-bytestring"; version = "0.1.1.7"; };
      license = "BSD-3-Clause";
      copyright = "Copyright 2011 MailRank, Inc.;\nCopyright 2010-2020 Bryan O'Sullivan et al.";
      maintainer = "Herbert Valerio Riedel <hvr@gnu.org>,\nMikhail Glushenkov <mikhail.glushenkov@gmail.com>,\nEmily Pillmore <emilypi@cohomolo.gy>";
      author = "Bryan O'Sullivan <bos@serpentine.com>";
      homepage = "http://github.com/haskell/base16-bytestring";
      url = "";
      synopsis = "Fast base16 (hex) encoding and decoding for ByteStrings";
      description = "This package provides support for encoding and decoding binary data according\nto @base16@ (see also <https://tools.ietf.org/html/rfc4648 RFC 4648>) for\nstrict (see \"Data.ByteString.Base16\") and lazy @ByteString@s (see \"Data.ByteString.Base16.Lazy\").\n\nSee also the <https://hackage.haskell.org/package/base-encoding base-encoding> package which\nprovides an uniform API providing conversion paths between more binary and textual types.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
          ];
        buildable = true;
        };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/base16-bytestring-0.1.1.7.tar.gz";
      sha256 = "525689679d5cc80fa532c1d5cfeace0f62bbb54134fad514f1ba00d0e7fe69ba";
      });
    }