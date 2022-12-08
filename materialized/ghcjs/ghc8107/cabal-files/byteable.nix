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
      specVersion = "1.8";
      identifier = { name = "byteable"; version = "0.1.1"; };
      license = "BSD-3-Clause";
      copyright = "Vincent Hanquez <vincent@snarc.org>";
      maintainer = "vincent@snarc.org";
      author = "Vincent Hanquez <vincent@snarc.org>";
      homepage = "http://github.com/vincenthz/hs-byteable";
      url = "";
      synopsis = "Type class for sequence of bytes";
      description = "Abstract class to manipulate sequence of bytes\n\nThe use case of this class is abstracting manipulation of\ntypes that are just wrapping a bytestring with stronger and\nmore meaniful name.\n\nUsual definition of those types are of the form: newtype MyType = MyType ByteString";
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
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/byteable-0.1.1.tar.gz";
      sha256 = "243b34a1b5b64b39e39fe58f75c18f6cad5b668b10cabcd86816cbde27783fe2";
      });
    }