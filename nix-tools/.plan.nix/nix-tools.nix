{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
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
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (builtins.throw "The Haskell package set does not contain the package: base (build dependency)"))
          (hsPkgs."hnix" or (builtins.throw "The Haskell package set does not contain the package: hnix (build dependency)"))
          (hsPkgs."aeson" or (builtins.throw "The Haskell package set does not contain the package: aeson (build dependency)"))
          (hsPkgs."unordered-containers" or (builtins.throw "The Haskell package set does not contain the package: unordered-containers (build dependency)"))
          (hsPkgs."process" or (builtins.throw "The Haskell package set does not contain the package: process (build dependency)"))
          (hsPkgs."deepseq" or (builtins.throw "The Haskell package set does not contain the package: deepseq (build dependency)"))
          (hsPkgs."transformers" or (builtins.throw "The Haskell package set does not contain the package: transformers (build dependency)"))
          (hsPkgs."data-fix" or (builtins.throw "The Haskell package set does not contain the package: data-fix (build dependency)"))
          (hsPkgs."Cabal" or (builtins.throw "The Haskell package set does not contain the package: Cabal (build dependency)"))
          (hsPkgs."text" or (builtins.throw "The Haskell package set does not contain the package: text (build dependency)"))
          (hsPkgs."filepath" or (builtins.throw "The Haskell package set does not contain the package: filepath (build dependency)"))
          (hsPkgs."directory" or (builtins.throw "The Haskell package set does not contain the package: directory (build dependency)"))
          (hsPkgs."bytestring" or (builtins.throw "The Haskell package set does not contain the package: bytestring (build dependency)"))
          (hsPkgs."cryptohash-sha256" or (builtins.throw "The Haskell package set does not contain the package: cryptohash-sha256 (build dependency)"))
          (hsPkgs."base16-bytestring" or (builtins.throw "The Haskell package set does not contain the package: base16-bytestring (build dependency)"))
          (hsPkgs."hpack" or (builtins.throw "The Haskell package set does not contain the package: hpack (build dependency)"))
          ];
        };
      exes = {
        "cabal-to-nix" = {
          depends = [
            (hsPkgs."base" or (builtins.throw "The Haskell package set does not contain the package: base (build dependency)"))
            (hsPkgs."transformers" or (builtins.throw "The Haskell package set does not contain the package: transformers (build dependency)"))
            (hsPkgs."bytestring" or (builtins.throw "The Haskell package set does not contain the package: bytestring (build dependency)"))
            (hsPkgs."hpack" or (builtins.throw "The Haskell package set does not contain the package: hpack (build dependency)"))
            (hsPkgs."hnix" or (builtins.throw "The Haskell package set does not contain the package: hnix (build dependency)"))
            (hsPkgs."text" or (builtins.throw "The Haskell package set does not contain the package: text (build dependency)"))
            (hsPkgs."nix-tools" or (builtins.throw "The Haskell package set does not contain the package: nix-tools (build dependency)"))
            (hsPkgs."filepath" or (builtins.throw "The Haskell package set does not contain the package: filepath (build dependency)"))
            (hsPkgs."directory" or (builtins.throw "The Haskell package set does not contain the package: directory (build dependency)"))
            (hsPkgs."prettyprinter" or (builtins.throw "The Haskell package set does not contain the package: prettyprinter (build dependency)"))
            ];
          };
        "hashes-to-nix" = {
          depends = [
            (hsPkgs."base" or (builtins.throw "The Haskell package set does not contain the package: base (build dependency)"))
            (hsPkgs."hnix" or (builtins.throw "The Haskell package set does not contain the package: hnix (build dependency)"))
            (hsPkgs."nix-tools" or (builtins.throw "The Haskell package set does not contain the package: nix-tools (build dependency)"))
            (hsPkgs."data-fix" or (builtins.throw "The Haskell package set does not contain the package: data-fix (build dependency)"))
            (hsPkgs."aeson" or (builtins.throw "The Haskell package set does not contain the package: aeson (build dependency)"))
            (hsPkgs."microlens" or (builtins.throw "The Haskell package set does not contain the package: microlens (build dependency)"))
            (hsPkgs."microlens-aeson" or (builtins.throw "The Haskell package set does not contain the package: microlens-aeson (build dependency)"))
            (hsPkgs."text" or (builtins.throw "The Haskell package set does not contain the package: text (build dependency)"))
            (hsPkgs."filepath" or (builtins.throw "The Haskell package set does not contain the package: filepath (build dependency)"))
            (hsPkgs."directory" or (builtins.throw "The Haskell package set does not contain the package: directory (build dependency)"))
            ];
          };
        "plan-to-nix" = {
          depends = [
            (hsPkgs."base" or (builtins.throw "The Haskell package set does not contain the package: base (build dependency)"))
            (hsPkgs."nix-tools" or (builtins.throw "The Haskell package set does not contain the package: nix-tools (build dependency)"))
            (hsPkgs."hnix" or (builtins.throw "The Haskell package set does not contain the package: hnix (build dependency)"))
            (hsPkgs."Cabal" or (builtins.throw "The Haskell package set does not contain the package: Cabal (build dependency)"))
            (hsPkgs."text" or (builtins.throw "The Haskell package set does not contain the package: text (build dependency)"))
            (hsPkgs."hpack" or (builtins.throw "The Haskell package set does not contain the package: hpack (build dependency)"))
            (hsPkgs."unordered-containers" or (builtins.throw "The Haskell package set does not contain the package: unordered-containers (build dependency)"))
            (hsPkgs."vector" or (builtins.throw "The Haskell package set does not contain the package: vector (build dependency)"))
            (hsPkgs."aeson" or (builtins.throw "The Haskell package set does not contain the package: aeson (build dependency)"))
            (hsPkgs."microlens" or (builtins.throw "The Haskell package set does not contain the package: microlens (build dependency)"))
            (hsPkgs."microlens-aeson" or (builtins.throw "The Haskell package set does not contain the package: microlens-aeson (build dependency)"))
            (hsPkgs."optparse-applicative" or (builtins.throw "The Haskell package set does not contain the package: optparse-applicative (build dependency)"))
            (hsPkgs."prettyprinter" or (builtins.throw "The Haskell package set does not contain the package: prettyprinter (build dependency)"))
            (hsPkgs."filepath" or (builtins.throw "The Haskell package set does not contain the package: filepath (build dependency)"))
            (hsPkgs."directory" or (builtins.throw "The Haskell package set does not contain the package: directory (build dependency)"))
            (hsPkgs."bytestring" or (builtins.throw "The Haskell package set does not contain the package: bytestring (build dependency)"))
            (hsPkgs."transformers" or (builtins.throw "The Haskell package set does not contain the package: transformers (build dependency)"))
            (hsPkgs."extra" or (builtins.throw "The Haskell package set does not contain the package: extra (build dependency)"))
            ];
          };
        "hackage-to-nix" = {
          depends = [
            (hsPkgs."base" or (builtins.throw "The Haskell package set does not contain the package: base (build dependency)"))
            (hsPkgs."nix-tools" or (builtins.throw "The Haskell package set does not contain the package: nix-tools (build dependency)"))
            (hsPkgs."hackage-db" or (builtins.throw "The Haskell package set does not contain the package: hackage-db (build dependency)"))
            (hsPkgs."hnix" or (builtins.throw "The Haskell package set does not contain the package: hnix (build dependency)"))
            (hsPkgs."Cabal" or (builtins.throw "The Haskell package set does not contain the package: Cabal (build dependency)"))
            (hsPkgs."containers" or (builtins.throw "The Haskell package set does not contain the package: containers (build dependency)"))
            (hsPkgs."bytestring" or (builtins.throw "The Haskell package set does not contain the package: bytestring (build dependency)"))
            (hsPkgs."text" or (builtins.throw "The Haskell package set does not contain the package: text (build dependency)"))
            (hsPkgs."cryptohash-sha256" or (builtins.throw "The Haskell package set does not contain the package: cryptohash-sha256 (build dependency)"))
            (hsPkgs."base16-bytestring" or (builtins.throw "The Haskell package set does not contain the package: base16-bytestring (build dependency)"))
            (hsPkgs."filepath" or (builtins.throw "The Haskell package set does not contain the package: filepath (build dependency)"))
            (hsPkgs."directory" or (builtins.throw "The Haskell package set does not contain the package: directory (build dependency)"))
            (hsPkgs."transformers" or (builtins.throw "The Haskell package set does not contain the package: transformers (build dependency)"))
            ];
          };
        "lts-to-nix" = {
          depends = [
            (hsPkgs."base" or (builtins.throw "The Haskell package set does not contain the package: base (build dependency)"))
            (hsPkgs."nix-tools" or (builtins.throw "The Haskell package set does not contain the package: nix-tools (build dependency)"))
            (hsPkgs."hnix" or (builtins.throw "The Haskell package set does not contain the package: hnix (build dependency)"))
            (hsPkgs."yaml" or (builtins.throw "The Haskell package set does not contain the package: yaml (build dependency)"))
            (hsPkgs."aeson" or (builtins.throw "The Haskell package set does not contain the package: aeson (build dependency)"))
            (hsPkgs."microlens" or (builtins.throw "The Haskell package set does not contain the package: microlens (build dependency)"))
            (hsPkgs."microlens-aeson" or (builtins.throw "The Haskell package set does not contain the package: microlens-aeson (build dependency)"))
            (hsPkgs."text" or (builtins.throw "The Haskell package set does not contain the package: text (build dependency)"))
            (hsPkgs."filepath" or (builtins.throw "The Haskell package set does not contain the package: filepath (build dependency)"))
            (hsPkgs."directory" or (builtins.throw "The Haskell package set does not contain the package: directory (build dependency)"))
            (hsPkgs."unordered-containers" or (builtins.throw "The Haskell package set does not contain the package: unordered-containers (build dependency)"))
            (hsPkgs."Cabal" or (builtins.throw "The Haskell package set does not contain the package: Cabal (build dependency)"))
            ];
          };
        "stack-to-nix" = {
          depends = [
            (hsPkgs."base" or (builtins.throw "The Haskell package set does not contain the package: base (build dependency)"))
            (hsPkgs."nix-tools" or (builtins.throw "The Haskell package set does not contain the package: nix-tools (build dependency)"))
            (hsPkgs."transformers" or (builtins.throw "The Haskell package set does not contain the package: transformers (build dependency)"))
            (hsPkgs."hnix" or (builtins.throw "The Haskell package set does not contain the package: hnix (build dependency)"))
            (hsPkgs."yaml" or (builtins.throw "The Haskell package set does not contain the package: yaml (build dependency)"))
            (hsPkgs."aeson" or (builtins.throw "The Haskell package set does not contain the package: aeson (build dependency)"))
            (hsPkgs."microlens" or (builtins.throw "The Haskell package set does not contain the package: microlens (build dependency)"))
            (hsPkgs."microlens-aeson" or (builtins.throw "The Haskell package set does not contain the package: microlens-aeson (build dependency)"))
            (hsPkgs."text" or (builtins.throw "The Haskell package set does not contain the package: text (build dependency)"))
            (hsPkgs."Cabal" or (builtins.throw "The Haskell package set does not contain the package: Cabal (build dependency)"))
            (hsPkgs."vector" or (builtins.throw "The Haskell package set does not contain the package: vector (build dependency)"))
            (hsPkgs."prettyprinter" or (builtins.throw "The Haskell package set does not contain the package: prettyprinter (build dependency)"))
            (hsPkgs."directory" or (builtins.throw "The Haskell package set does not contain the package: directory (build dependency)"))
            (hsPkgs."filepath" or (builtins.throw "The Haskell package set does not contain the package: filepath (build dependency)"))
            (hsPkgs."extra" or (builtins.throw "The Haskell package set does not contain the package: extra (build dependency)"))
            (hsPkgs."hpack" or (builtins.throw "The Haskell package set does not contain the package: hpack (build dependency)"))
            (hsPkgs."bytestring" or (builtins.throw "The Haskell package set does not contain the package: bytestring (build dependency)"))
            (hsPkgs."optparse-applicative" or (builtins.throw "The Haskell package set does not contain the package: optparse-applicative (build dependency)"))
            (hsPkgs."http-client-tls" or (builtins.throw "The Haskell package set does not contain the package: http-client-tls (build dependency)"))
            (hsPkgs."http-client" or (builtins.throw "The Haskell package set does not contain the package: http-client (build dependency)"))
            (hsPkgs."http-types" or (builtins.throw "The Haskell package set does not contain the package: http-types (build dependency)"))
            (hsPkgs."unordered-containers" or (builtins.throw "The Haskell package set does not contain the package: unordered-containers (build dependency)"))
            ];
          };
        "truncate-index" = {
          depends = [
            (hsPkgs."base" or (builtins.throw "The Haskell package set does not contain the package: base (build dependency)"))
            (hsPkgs."optparse-applicative" or (builtins.throw "The Haskell package set does not contain the package: optparse-applicative (build dependency)"))
            (hsPkgs."zlib" or (builtins.throw "The Haskell package set does not contain the package: zlib (build dependency)"))
            (hsPkgs."tar" or (builtins.throw "The Haskell package set does not contain the package: tar (build dependency)"))
            (hsPkgs."bytestring" or (builtins.throw "The Haskell package set does not contain the package: bytestring (build dependency)"))
            (hsPkgs."time" or (builtins.throw "The Haskell package set does not contain the package: time (build dependency)"))
            ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../../../nix-tools/.; }