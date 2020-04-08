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
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = { name = "nix-tools"; version = "0.1.0.0"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "moritz.angermann@gmail.com";
      author = "Moritz Angermann";
      homepage = "";
      url = "";
      synopsis = "cabal/stack to nix translation tools";
      description = "A set of tools to aid in trating stack and cabal projects into nix expressions.";
      buildType = "Simple";
      isLocal = true;
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (buildDepError "base"))
          (hsPkgs."Cabal" or (buildDepError "Cabal"))
          (hsPkgs."aeson" or (buildDepError "aeson"))
          (hsPkgs."aeson-pretty" or (buildDepError "aeson-pretty"))
          (hsPkgs."base16-bytestring" or (buildDepError "base16-bytestring"))
          (hsPkgs."bytestring" or (buildDepError "bytestring"))
          (hsPkgs."cryptohash-sha256" or (buildDepError "cryptohash-sha256"))
          (hsPkgs."containers" or (buildDepError "containers"))
          (hsPkgs."data-fix" or (buildDepError "data-fix"))
          (hsPkgs."deepseq" or (buildDepError "deepseq"))
          (hsPkgs."directory" or (buildDepError "directory"))
          (hsPkgs."extra" or (buildDepError "extra"))
          (hsPkgs."filepath" or (buildDepError "filepath"))
          (hsPkgs."hnix" or (buildDepError "hnix"))
          (hsPkgs."hpack" or (buildDepError "hpack"))
          (hsPkgs."http-client" or (buildDepError "http-client"))
          (hsPkgs."http-client-tls" or (buildDepError "http-client-tls"))
          (hsPkgs."http-types" or (buildDepError "http-types"))
          (hsPkgs."optparse-applicative" or (buildDepError "optparse-applicative"))
          (hsPkgs."prettyprinter" or (buildDepError "prettyprinter"))
          (hsPkgs."process" or (buildDepError "process"))
          (hsPkgs."text" or (buildDepError "text"))
          (hsPkgs."transformers" or (buildDepError "transformers"))
          (hsPkgs."unordered-containers" or (buildDepError "unordered-containers"))
          (hsPkgs."yaml" or (buildDepError "yaml"))
          ];
        buildable = true;
        };
      exes = {
        "cabal-to-nix" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."transformers" or (buildDepError "transformers"))
            (hsPkgs."bytestring" or (buildDepError "bytestring"))
            (hsPkgs."hpack" or (buildDepError "hpack"))
            (hsPkgs."hnix" or (buildDepError "hnix"))
            (hsPkgs."text" or (buildDepError "text"))
            (hsPkgs."nix-tools" or (buildDepError "nix-tools"))
            (hsPkgs."filepath" or (buildDepError "filepath"))
            (hsPkgs."directory" or (buildDepError "directory"))
            (hsPkgs."prettyprinter" or (buildDepError "prettyprinter"))
            ];
          buildable = true;
          };
        "hashes-to-nix" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."hnix" or (buildDepError "hnix"))
            (hsPkgs."nix-tools" or (buildDepError "nix-tools"))
            (hsPkgs."data-fix" or (buildDepError "data-fix"))
            (hsPkgs."aeson" or (buildDepError "aeson"))
            (hsPkgs."microlens" or (buildDepError "microlens"))
            (hsPkgs."microlens-aeson" or (buildDepError "microlens-aeson"))
            (hsPkgs."text" or (buildDepError "text"))
            (hsPkgs."filepath" or (buildDepError "filepath"))
            (hsPkgs."directory" or (buildDepError "directory"))
            ];
          buildable = true;
          };
        "plan-to-nix" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."nix-tools" or (buildDepError "nix-tools"))
            (hsPkgs."hnix" or (buildDepError "hnix"))
            (hsPkgs."Cabal" or (buildDepError "Cabal"))
            (hsPkgs."text" or (buildDepError "text"))
            (hsPkgs."hpack" or (buildDepError "hpack"))
            (hsPkgs."unordered-containers" or (buildDepError "unordered-containers"))
            (hsPkgs."vector" or (buildDepError "vector"))
            (hsPkgs."aeson" or (buildDepError "aeson"))
            (hsPkgs."microlens" or (buildDepError "microlens"))
            (hsPkgs."microlens-aeson" or (buildDepError "microlens-aeson"))
            (hsPkgs."optparse-applicative" or (buildDepError "optparse-applicative"))
            (hsPkgs."prettyprinter" or (buildDepError "prettyprinter"))
            (hsPkgs."filepath" or (buildDepError "filepath"))
            (hsPkgs."directory" or (buildDepError "directory"))
            (hsPkgs."bytestring" or (buildDepError "bytestring"))
            (hsPkgs."transformers" or (buildDepError "transformers"))
            (hsPkgs."extra" or (buildDepError "extra"))
            ];
          buildable = true;
          };
        "hackage-to-nix" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."nix-tools" or (buildDepError "nix-tools"))
            (hsPkgs."hackage-db" or (buildDepError "hackage-db"))
            (hsPkgs."hnix" or (buildDepError "hnix"))
            (hsPkgs."Cabal" or (buildDepError "Cabal"))
            (hsPkgs."aeson" or (buildDepError "aeson"))
            (hsPkgs."aeson-pretty" or (buildDepError "aeson-pretty"))
            (hsPkgs."containers" or (buildDepError "containers"))
            (hsPkgs."bytestring" or (buildDepError "bytestring"))
            (hsPkgs."text" or (buildDepError "text"))
            (hsPkgs."cryptohash-sha256" or (buildDepError "cryptohash-sha256"))
            (hsPkgs."base16-bytestring" or (buildDepError "base16-bytestring"))
            (hsPkgs."base64-bytestring" or (buildDepError "base64-bytestring"))
            (hsPkgs."filepath" or (buildDepError "filepath"))
            (hsPkgs."directory" or (buildDepError "directory"))
            (hsPkgs."transformers" or (buildDepError "transformers"))
            ];
          buildable = true;
          };
        "lts-to-nix" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."nix-tools" or (buildDepError "nix-tools"))
            (hsPkgs."hnix" or (buildDepError "hnix"))
            (hsPkgs."yaml" or (buildDepError "yaml"))
            (hsPkgs."aeson" or (buildDepError "aeson"))
            (hsPkgs."microlens" or (buildDepError "microlens"))
            (hsPkgs."microlens-aeson" or (buildDepError "microlens-aeson"))
            (hsPkgs."text" or (buildDepError "text"))
            (hsPkgs."filepath" or (buildDepError "filepath"))
            (hsPkgs."directory" or (buildDepError "directory"))
            (hsPkgs."unordered-containers" or (buildDepError "unordered-containers"))
            (hsPkgs."vector" or (buildDepError "vector"))
            (hsPkgs."Cabal" or (buildDepError "Cabal"))
            ];
          buildable = true;
          };
        "stack-to-nix" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."nix-tools" or (buildDepError "nix-tools"))
            ];
          buildable = true;
          };
        "truncate-index" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."optparse-applicative" or (buildDepError "optparse-applicative"))
            (hsPkgs."zlib" or (buildDepError "zlib"))
            (hsPkgs."tar" or (buildDepError "tar"))
            (hsPkgs."bytestring" or (buildDepError "bytestring"))
            (hsPkgs."time" or (buildDepError "time"))
            ];
          buildable = true;
          };
        "stack-repos" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."nix-tools" or (buildDepError "nix-tools"))
            ];
          buildable = true;
          };
        "cabal-name" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."nix-tools" or (buildDepError "nix-tools"))
            ];
          buildable = true;
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../.; }