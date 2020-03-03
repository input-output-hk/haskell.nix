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
    flags = { use-host-template-haskell = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "ghcjs-th"; version = "0.1.0.0"; };
      license = "MIT";
      copyright = "";
      maintainer = "stegeman@gmail.com";
      author = "Luite Stegeman";
      homepage = "http://github.com/ghcjs";
      url = "";
      synopsis = "";
      description = "";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" ];
      dataDir = "";
      dataFiles = [];
      extraSrcFiles = [];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (buildDepError "base"))
          (hsPkgs."ghc-prim" or (buildDepError "ghc-prim"))
          (hsPkgs."binary" or (buildDepError "binary"))
          (hsPkgs."bytestring" or (buildDepError "bytestring"))
          (hsPkgs."containers" or (buildDepError "containers"))
          ] ++ (if compiler.isGhcjs && true || flags.use-host-template-haskell
          then [
            (hsPkgs."template-haskell" or (buildDepError "template-haskell"))
            (hsPkgs."ghci" or (buildDepError "ghci"))
            ]
          else [
            (hsPkgs."template-haskell-ghcjs" or (buildDepError "template-haskell-ghcjs"))
            (hsPkgs."ghci-ghcjs" or (buildDepError "ghci-ghcjs"))
            ]);
        buildable = true;
        modules = [
          "GHCJS/Prim/TH/Eval"
          "GHCJS/Prim/TH/Serialized"
          "GHCJS/Prim/TH/Types"
          ];
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../lib/ghcjs-th; }