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
in { system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, config, ... }:
  {
    flags = { install-examples = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "hackage-db"; version = "2.1.0"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Peter Simons <simons@cryp.to>";
      author = "Peter Simons, Alexander Altman, Ben James";
      homepage = "https://github.com/peti/hackage-db#readme";
      url = "";
      synopsis = "Access cabal-install's Hackage database via Data.Map";
      description = "This library provides convenient access to the local copy of the Hackage\ndatabase that \\\"cabal update\\\" creates. Check out\n<https://github.com/peti/hackage-db/tree/master/example/> for a collection\nof simple example programs that demonstrate how to use this code.";
      buildType = "Simple";
      isLocal = true;
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (buildDepError "base"))
          (hsPkgs."Cabal" or (buildDepError "Cabal"))
          (hsPkgs."aeson" or (buildDepError "aeson"))
          (hsPkgs."bytestring" or (buildDepError "bytestring"))
          (hsPkgs."containers" or (buildDepError "containers"))
          (hsPkgs."directory" or (buildDepError "directory"))
          (hsPkgs."exceptions" or (buildDepError "exceptions"))
          (hsPkgs."filepath" or (buildDepError "filepath"))
          (hsPkgs."tar" or (buildDepError "tar"))
          (hsPkgs."time" or (buildDepError "time"))
          (hsPkgs."utf8-string" or (buildDepError "utf8-string"))
          ];
        buildable = true;
        };
      exes = {
        "list-known-versions" = {
          depends = (pkgs.lib).optionals (flags.install-examples) [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."Cabal" or (buildDepError "Cabal"))
            (hsPkgs."bytestring" or (buildDepError "bytestring"))
            (hsPkgs."containers" or (buildDepError "containers"))
            (hsPkgs."hackage-db" or (buildDepError "hackage-db"))
            ];
          buildable = if flags.install-examples then true else false;
          };
        "show-meta-data" = {
          depends = (pkgs.lib).optionals (flags.install-examples) [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."Cabal" or (buildDepError "Cabal"))
            (hsPkgs."containers" or (buildDepError "containers"))
            (hsPkgs."hackage-db" or (buildDepError "hackage-db"))
            (hsPkgs."utf8-string" or (buildDepError "utf8-string"))
            ];
          buildable = if flags.install-examples then true else false;
          };
        "show-package-versions" = {
          depends = (pkgs.lib).optionals (flags.install-examples) [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."Cabal" or (buildDepError "Cabal"))
            (hsPkgs."containers" or (buildDepError "containers"))
            (hsPkgs."hackage-db" or (buildDepError "hackage-db"))
            ];
          buildable = if flags.install-examples then true else false;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/hamishmack/hackage-db.git";
      rev = "f1f528db7b02d90e9953297c716de5e046f3570f";
      sha256 = "0j86xkpndxn41sy2mchx029zc3isaxh61hdvnakpiflkcmfaji1w";
      });
    }