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
      specVersion = "1.6";
      identifier = { name = "asn1-types"; version = "0.3.4"; };
      license = "BSD-3-Clause";
      copyright = "Vincent Hanquez <vincent@snarc.org>";
      maintainer = "Vincent Hanquez <vincent@snarc.org>";
      author = "Vincent Hanquez <vincent@snarc.org>";
      homepage = "http://github.com/vincenthz/hs-asn1";
      url = "";
      synopsis = "ASN.1 types";
      description = "ASN.1 standard types";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."memory" or (errorHandler.buildDepError "memory"))
          (hsPkgs."hourglass" or (errorHandler.buildDepError "hourglass"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/asn1-types-0.3.4.tar.gz";
      sha256 = "78ee92a251379298ca820fa53edbf4b33c539b9fcd887c86f520c30e3b4e21a8";
      });
    }