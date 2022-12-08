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
      identifier = { name = "appar"; version = "0.1.8"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Kazu Yamamoto <kazu@iij.ad.jp>";
      author = "Kazu Yamamoto <kazu@iij.ad.jp>";
      homepage = "";
      url = "";
      synopsis = "A simple applicative parser";
      description = "A simple applicative parser in Parsec style";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          ] ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8.0")) (hsPkgs."fail" or (errorHandler.buildDepError "fail"));
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/appar-0.1.8.tar.gz";
      sha256 = "c4ceeddc26525b58d82c41b6d3e32141371a200a6794aae185b6266ccc81631f";
      });
    }