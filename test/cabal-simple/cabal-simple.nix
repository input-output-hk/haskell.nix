{ system
, compiler
, flags
, pkgs
, hsPkgs
, pkgconfPkgs
, ... }:
  {
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = {
        name = "cabal-simple";
        version = "0.1.0.0";
      };
      license = "LicenseRef-PublicDomain";
      copyright = "";
      maintainer = "rodney.lorrimar@iohk.io";
      author = "Rodney Lorrimar";
      homepage = "";
      url = "";
      synopsis = "";
      description = "";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [ (hsPkgs.base) ];
      };
      exes = {
        "cabal-simple" = {
          depends = [ (hsPkgs.base) ];
        };
      };
    };
  } // rec {
    src = pkgs.lib.mkDefault ./.;
  }
