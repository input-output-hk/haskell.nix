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
      identifier = { name = "wl-pprint-text"; version = "1.2.0.1"; };
      license = "BSD-3-Clause";
      copyright = "2007 Daan Leijen, 2010 Ivan Lazar Miljenovic";
      maintainer = "Ivan.Miljenovic@gmail.com";
      author = "Ivan Lazar Miljenovic";
      homepage = "";
      url = "";
      synopsis = "A Wadler/Leijen Pretty Printer for Text values";
      description = "A clone of wl-pprint for use with the text library.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."base-compat" or (errorHandler.buildDepError "base-compat"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/wl-pprint-text-1.2.0.1.tar.gz";
      sha256 = "9e6efdba61da70caf85560570648ec097b88cc2b92bc1306135b93f0ff9b0c0c";
      });
    }