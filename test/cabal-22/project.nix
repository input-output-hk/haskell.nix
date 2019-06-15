{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = {};
    package = {
      specVersion = "2.2";
      identifier = { name = "project"; version = "0.1.0.0"; };
      license = "NONE";
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
      "library" = { depends = [ (hsPkgs.base) ]; };
      exes = { "project" = { depends = [ (hsPkgs.base) (hsPkgs.project) ]; }; };
      tests = {
        "unit" = {
          depends = [ (hsPkgs.base) (hsPkgs.project) (hsPkgs.hspec) ];
          build-tools = [
            (hsPkgs.buildPackages.hspec-discover or (pkgs.buildPackages.hspec-discover))
            ];
          };
        };
      benchmarks = {
        "project-bench" = { depends = [ (hsPkgs.base) (hsPkgs.project) ]; };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ./.; }
