let
  buildDepError = pkg:
    builtins.throw ''
      The Haskell package set does not contain the package: ${pkg} (build dependency).

      If you are using Stackage, make sure that you are using a snapshot that contains the package. Otherwise you may need to update the Hackage snapshot you are using, usually by updating haskell.nix.
      '';
in { system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, config, ... }:
  {
    flags = {};
    package = {
      specVersion = "2.4";
      identifier = { name = "external-package-demo"; version = "0.1.0.0"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "kolmax94@gmail.com";
      author = "Maxim Koltsov";
      homepage = "";
      url = "";
      synopsis = "Demo package.";
      description = "Just a demo package...";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [ (hsPkgs."base" or (buildDepError "base")) ];
        buildable = true;
        };
      exes = {
        "external-package-demo" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."external-package-demo" or (buildDepError "external-package-demo"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://not-there//package/external-package-demo-0.1.0.0/external-package-demo-0.1.0.0.tar.gz";
      sha256 = config.sha256;
      });
    }
