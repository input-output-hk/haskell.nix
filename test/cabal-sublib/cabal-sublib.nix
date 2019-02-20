{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = {};
    package = {
      specVersion = "2.2";
      identifier = { name = "cabal-sublib"; version = "0.1.0.0"; };
      license = "MIT";
      copyright = "";
      maintainer = "moritz.angermann@iohk.io";
      author = "Moritz Angermann";
      homepage = "";
      url = "";
      synopsis = "";
      description = "";
      buildType = "Simple";
      };
    components = {
      "library" = { depends = [ (hsPkgs.base) (hsPkgs.slib) ]; };
      sublibs = {
        "slib" = { depends = [ (hsPkgs.extra) (hsPkgs.safe) (hsPkgs.aeson) ]; };
        };
      exes = {
        "cabal-sublib" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.cabal-sublib)
            (hsPkgs.extra)
            (hsPkgs.optparse-applicative)
            ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ./.; }
