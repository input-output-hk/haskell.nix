let
  buildDepError = pkg:
    builtins.throw ''
      The Haskell package set does not contain the package: ${pkg} (build dependency).
      
      If you are using Stackage, make sure that you are using a snapshot that contains the package. Otherwise you may need to update the Hackage snapshot you are using, usually by updating haskell.nix.
      '';
  sysDepError = pkg:
    builtins.throw ''
      The Nixpkgs package set does not contain the package: ${pkg} (system dependency).
      
      You may need to augment the system package mapping in haskell.nix so that it can be found.
      '';
  pkgConfDepError = pkg:
    builtins.throw ''
      The pkg-conf packages does not contain the package: ${pkg} (pkg-conf dependency).
      
      You may need to augment the pkg-conf package mapping in haskell.nix so that it can be found.
      '';
  exeDepError = pkg:
    builtins.throw ''
      The local executable components do not include the component: ${pkg} (executable dependency).
      '';
  legacyExeDepError = pkg:
    builtins.throw ''
      The Haskell package set does not contain the package: ${pkg} (executable dependency).
      
      If you are using Stackage, make sure that you are using a snapshot that contains the package. Otherwise you may need to update the Hackage snapshot you are using, usually by updating haskell.nix.
      '';
  buildToolDepError = pkg:
    builtins.throw ''
      Neither the Haskell package set or the Nixpkgs package set contain the package: ${pkg} (build tool dependency).
      
      If this is a system dependency:
      You may need to augment the system package mapping in haskell.nix so that it can be found.
      
      If this is a Haskell dependency:
      If you are using Stackage, make sure that you are using a snapshot that contains the package. Otherwise you may need to update the Hackage snapshot you are using, usually by updating haskell.nix.
      '';
in { system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = { integer-simple = false; integer-gmp = false; };
    package = {
      specVersion = "2.1";
      identifier = { name = "base"; version = "4.12.0.0"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "libraries@haskell.org";
      author = "";
      homepage = "";
      url = "";
      synopsis = "Basic libraries";
      description = "This package contains the Standard Haskell \"Prelude\" and its support libraries,\nand a large collection of useful libraries ranging from data\nstructures to parsing combinators and debugging utilities.";
      buildType = "Configure";
      };
    components = {
      "library" = {
        depends = (([
          (hsPkgs."rts" or (buildDepError "rts"))
          (hsPkgs."ghc-prim" or (buildDepError "ghc-prim"))
          ] ++ (pkgs.lib).optional (!(flags.integer-gmp && !flags.integer-simple || !flags.integer-gmp && flags.integer-simple)) (hsPkgs."invalid-cabal-flag-settings" or (buildDepError "invalid-cabal-flag-settings"))) ++ (pkgs.lib).optional (flags.integer-simple) (hsPkgs."integer-simple" or (buildDepError "integer-simple"))) ++ (pkgs.lib).optional (flags.integer-gmp) (hsPkgs."integer-gmp" or (buildDepError "integer-gmp"));
        libs = (pkgs.lib).optionals (system.isWindows) [
          (pkgs."wsock32" or (sysDepError "wsock32"))
          (pkgs."user32" or (sysDepError "user32"))
          (pkgs."shell32" or (sysDepError "shell32"))
          (pkgs."msvcrt" or (sysDepError "msvcrt"))
          (pkgs."mingw32" or (sysDepError "mingw32"))
          (pkgs."mingwex" or (sysDepError "mingwex"))
          ];
        buildable = true;
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ./.; }
