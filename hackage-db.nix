{ system
, compiler
, flags
, pkgs
, hsPkgs
, pkgconfPkgs }:
  {
    flags = {
      install-examples = false;
    };
    package = {
      specVersion = "1.10";
      identifier = {
        name = "hackage-db";
        version = "2.0.1";
      };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Peter Simons <simons@cryp.to>";
      author = "Peter Simons, Alexander Altman, Ben James";
      homepage = "https://github.com/peti/hackage-db#readme";
      url = "";
      synopsis = "Access cabal-install's Hackage database via Data.Map";
      description = "This library provides convenient access to the local copy of the Hackage\ndatabase that \"cabal update\" creates. Check out\nhttps://github.com/peti/hackage-db/tree/master/example/ for a collection of\nsimple example programs that demonstrate how to use this code.";
      buildType = "Simple";
    };
    components = {
      "hackage-db" = {
        depends  = [
          (hsPkgs.base)
          (hsPkgs.Cabal)
          (hsPkgs.containers)
          (hsPkgs.aeson)
          (hsPkgs.bytestring)
          (hsPkgs.directory)
          (hsPkgs.filepath)
          (hsPkgs.tar)
          (hsPkgs.time)
          (hsPkgs.utf8-string)
        ];
      };
      exes = {
        "list-known-versions" = {
          depends  = pkgs.lib.optionals (flags.install-examples) [
            (hsPkgs.base)
            (hsPkgs.Cabal)
            (hsPkgs.containers)
            (hsPkgs.hackage-db)
            (hsPkgs.bytestring)
          ];
        };
        "show-meta-data" = {
          depends  = pkgs.lib.optionals (flags.install-examples) [
            (hsPkgs.base)
            (hsPkgs.Cabal)
            (hsPkgs.containers)
            (hsPkgs.hackage-db)
            (hsPkgs.utf8-string)
          ];
        };
        "show-package-versions" = {
          depends  = pkgs.lib.optionals (flags.install-examples) [
            (hsPkgs.base)
            (hsPkgs.Cabal)
            (hsPkgs.containers)
            (hsPkgs.hackage-db)
          ];
        };
      };
    };
  } // rec { src = ./hackage-db; }
