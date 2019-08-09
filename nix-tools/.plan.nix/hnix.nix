{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = { optimize = false; profiling = false; tracing = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "hnix"; version = "0.5.2"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "johnw@newartisans.com";
      author = "John Wiegley";
      homepage = "https://github.com/haskell-nix/hnix#readme";
      url = "";
      synopsis = "Haskell implementation of the Nix language";
      description = "Haskell implementation of the Nix language.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = (((([
          (hsPkgs."aeson" or (builtins.throw "The Haskell package set does not contain the package: aeson (build dependency)"))
          (hsPkgs."array" or (builtins.throw "The Haskell package set does not contain the package: array (build dependency)"))
          (hsPkgs."base" or (builtins.throw "The Haskell package set does not contain the package: base (build dependency)"))
          (hsPkgs."binary" or (builtins.throw "The Haskell package set does not contain the package: binary (build dependency)"))
          (hsPkgs."bytestring" or (builtins.throw "The Haskell package set does not contain the package: bytestring (build dependency)"))
          (hsPkgs."containers" or (builtins.throw "The Haskell package set does not contain the package: containers (build dependency)"))
          (hsPkgs."data-fix" or (builtins.throw "The Haskell package set does not contain the package: data-fix (build dependency)"))
          (hsPkgs."deepseq" or (builtins.throw "The Haskell package set does not contain the package: deepseq (build dependency)"))
          (hsPkgs."dependent-sum" or (builtins.throw "The Haskell package set does not contain the package: dependent-sum (build dependency)"))
          (hsPkgs."deriving-compat" or (builtins.throw "The Haskell package set does not contain the package: deriving-compat (build dependency)"))
          (hsPkgs."directory" or (builtins.throw "The Haskell package set does not contain the package: directory (build dependency)"))
          (hsPkgs."exceptions" or (builtins.throw "The Haskell package set does not contain the package: exceptions (build dependency)"))
          (hsPkgs."filepath" or (builtins.throw "The Haskell package set does not contain the package: filepath (build dependency)"))
          (hsPkgs."free" or (builtins.throw "The Haskell package set does not contain the package: free (build dependency)"))
          (hsPkgs."hashing" or (builtins.throw "The Haskell package set does not contain the package: hashing (build dependency)"))
          (hsPkgs."http-client" or (builtins.throw "The Haskell package set does not contain the package: http-client (build dependency)"))
          (hsPkgs."http-client-tls" or (builtins.throw "The Haskell package set does not contain the package: http-client-tls (build dependency)"))
          (hsPkgs."http-types" or (builtins.throw "The Haskell package set does not contain the package: http-types (build dependency)"))
          (hsPkgs."interpolate" or (builtins.throw "The Haskell package set does not contain the package: interpolate (build dependency)"))
          (hsPkgs."lens-family-th" or (builtins.throw "The Haskell package set does not contain the package: lens-family-th (build dependency)"))
          (hsPkgs."logict" or (builtins.throw "The Haskell package set does not contain the package: logict (build dependency)"))
          (hsPkgs."megaparsec" or (builtins.throw "The Haskell package set does not contain the package: megaparsec (build dependency)"))
          (hsPkgs."monadlist" or (builtins.throw "The Haskell package set does not contain the package: monadlist (build dependency)"))
          (hsPkgs."mtl" or (builtins.throw "The Haskell package set does not contain the package: mtl (build dependency)"))
          (hsPkgs."optparse-applicative" or (builtins.throw "The Haskell package set does not contain the package: optparse-applicative (build dependency)"))
          (hsPkgs."parser-combinators" or (builtins.throw "The Haskell package set does not contain the package: parser-combinators (build dependency)"))
          (hsPkgs."prettyprinter" or (builtins.throw "The Haskell package set does not contain the package: prettyprinter (build dependency)"))
          (hsPkgs."process" or (builtins.throw "The Haskell package set does not contain the package: process (build dependency)"))
          (hsPkgs."ref-tf" or (builtins.throw "The Haskell package set does not contain the package: ref-tf (build dependency)"))
          (hsPkgs."regex-tdfa" or (builtins.throw "The Haskell package set does not contain the package: regex-tdfa (build dependency)"))
          (hsPkgs."regex-tdfa-text" or (builtins.throw "The Haskell package set does not contain the package: regex-tdfa-text (build dependency)"))
          (hsPkgs."scientific" or (builtins.throw "The Haskell package set does not contain the package: scientific (build dependency)"))
          (hsPkgs."semigroups" or (builtins.throw "The Haskell package set does not contain the package: semigroups (build dependency)"))
          (hsPkgs."split" or (builtins.throw "The Haskell package set does not contain the package: split (build dependency)"))
          (hsPkgs."syb" or (builtins.throw "The Haskell package set does not contain the package: syb (build dependency)"))
          (hsPkgs."template-haskell" or (builtins.throw "The Haskell package set does not contain the package: template-haskell (build dependency)"))
          (hsPkgs."text" or (builtins.throw "The Haskell package set does not contain the package: text (build dependency)"))
          (hsPkgs."these" or (builtins.throw "The Haskell package set does not contain the package: these (build dependency)"))
          (hsPkgs."time" or (builtins.throw "The Haskell package set does not contain the package: time (build dependency)"))
          (hsPkgs."transformers" or (builtins.throw "The Haskell package set does not contain the package: transformers (build dependency)"))
          (hsPkgs."unix" or (builtins.throw "The Haskell package set does not contain the package: unix (build dependency)"))
          (hsPkgs."unordered-containers" or (builtins.throw "The Haskell package set does not contain the package: unordered-containers (build dependency)"))
          (hsPkgs."vector" or (builtins.throw "The Haskell package set does not contain the package: vector (build dependency)"))
          (hsPkgs."xml" or (builtins.throw "The Haskell package set does not contain the package: xml (build dependency)"))
          ] ++ (pkgs.lib).optional (system.isLinux && (compiler.isGhc && (compiler.version).ge "8.2") && (compiler.isGhc && (compiler.version).lt "8.3")) (hsPkgs."compact" or (builtins.throw "The Haskell package set does not contain the package: compact (build dependency)"))) ++ (pkgs.lib).optionals (!(compiler.isGhcjs && true)) [
          (hsPkgs."base16-bytestring" or (builtins.throw "The Haskell package set does not contain the package: base16-bytestring (build dependency)"))
          (hsPkgs."cryptohash-md5" or (builtins.throw "The Haskell package set does not contain the package: cryptohash-md5 (build dependency)"))
          (hsPkgs."cryptohash-sha1" or (builtins.throw "The Haskell package set does not contain the package: cryptohash-sha1 (build dependency)"))
          (hsPkgs."cryptohash-sha256" or (builtins.throw "The Haskell package set does not contain the package: cryptohash-sha256 (build dependency)"))
          (hsPkgs."cryptohash-sha512" or (builtins.throw "The Haskell package set does not contain the package: cryptohash-sha512 (build dependency)"))
          (hsPkgs."serialise" or (builtins.throw "The Haskell package set does not contain the package: serialise (build dependency)"))
          ]) ++ [
          (hsPkgs."lens-family" or (builtins.throw "The Haskell package set does not contain the package: lens-family (build dependency)"))
          (hsPkgs."lens-family-core" or (builtins.throw "The Haskell package set does not contain the package: lens-family-core (build dependency)"))
          ]) ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).lt "8.4.0" && !flags.profiling) (hsPkgs."ghc-datasize" or (builtins.throw "The Haskell package set does not contain the package: ghc-datasize (build dependency)"))) ++ (if compiler.isGhcjs && true
          then [
            (hsPkgs."hashable" or (builtins.throw "The Haskell package set does not contain the package: hashable (build dependency)"))
            ]
          else [
            (hsPkgs."hashable" or (builtins.throw "The Haskell package set does not contain the package: hashable (build dependency)"))
            (hsPkgs."haskeline" or (builtins.throw "The Haskell package set does not contain the package: haskeline (build dependency)"))
            (hsPkgs."pretty-show" or (builtins.throw "The Haskell package set does not contain the package: pretty-show (build dependency)"))
            ]);
        };
      exes = {
        "hnix" = {
          depends = ([
            (hsPkgs."aeson" or (builtins.throw "The Haskell package set does not contain the package: aeson (build dependency)"))
            (hsPkgs."base" or (builtins.throw "The Haskell package set does not contain the package: base (build dependency)"))
            (hsPkgs."bytestring" or (builtins.throw "The Haskell package set does not contain the package: bytestring (build dependency)"))
            (hsPkgs."containers" or (builtins.throw "The Haskell package set does not contain the package: containers (build dependency)"))
            (hsPkgs."data-fix" or (builtins.throw "The Haskell package set does not contain the package: data-fix (build dependency)"))
            (hsPkgs."deepseq" or (builtins.throw "The Haskell package set does not contain the package: deepseq (build dependency)"))
            (hsPkgs."exceptions" or (builtins.throw "The Haskell package set does not contain the package: exceptions (build dependency)"))
            (hsPkgs."filepath" or (builtins.throw "The Haskell package set does not contain the package: filepath (build dependency)"))
            (hsPkgs."hashing" or (builtins.throw "The Haskell package set does not contain the package: hashing (build dependency)"))
            (hsPkgs."haskeline" or (builtins.throw "The Haskell package set does not contain the package: haskeline (build dependency)"))
            (hsPkgs."hnix" or (builtins.throw "The Haskell package set does not contain the package: hnix (build dependency)"))
            (hsPkgs."mtl" or (builtins.throw "The Haskell package set does not contain the package: mtl (build dependency)"))
            (hsPkgs."optparse-applicative" or (builtins.throw "The Haskell package set does not contain the package: optparse-applicative (build dependency)"))
            (hsPkgs."pretty-show" or (builtins.throw "The Haskell package set does not contain the package: pretty-show (build dependency)"))
            (hsPkgs."prettyprinter" or (builtins.throw "The Haskell package set does not contain the package: prettyprinter (build dependency)"))
            (hsPkgs."repline" or (builtins.throw "The Haskell package set does not contain the package: repline (build dependency)"))
            (hsPkgs."template-haskell" or (builtins.throw "The Haskell package set does not contain the package: template-haskell (build dependency)"))
            (hsPkgs."text" or (builtins.throw "The Haskell package set does not contain the package: text (build dependency)"))
            (hsPkgs."time" or (builtins.throw "The Haskell package set does not contain the package: time (build dependency)"))
            (hsPkgs."transformers" or (builtins.throw "The Haskell package set does not contain the package: transformers (build dependency)"))
            (hsPkgs."unordered-containers" or (builtins.throw "The Haskell package set does not contain the package: unordered-containers (build dependency)"))
            ] ++ (pkgs.lib).optional (system.isLinux && (compiler.isGhc && (compiler.version).ge "8.2") && (compiler.isGhc && (compiler.version).lt "8.3")) (hsPkgs."compact" or (builtins.throw "The Haskell package set does not contain the package: compact (build dependency)"))) ++ (pkgs.lib).optionals (!(compiler.isGhcjs && true)) [
            (hsPkgs."base16-bytestring" or (builtins.throw "The Haskell package set does not contain the package: base16-bytestring (build dependency)"))
            (hsPkgs."cryptohash-md5" or (builtins.throw "The Haskell package set does not contain the package: cryptohash-md5 (build dependency)"))
            (hsPkgs."cryptohash-sha1" or (builtins.throw "The Haskell package set does not contain the package: cryptohash-sha1 (build dependency)"))
            (hsPkgs."cryptohash-sha256" or (builtins.throw "The Haskell package set does not contain the package: cryptohash-sha256 (build dependency)"))
            (hsPkgs."cryptohash-sha512" or (builtins.throw "The Haskell package set does not contain the package: cryptohash-sha512 (build dependency)"))
            (hsPkgs."serialise" or (builtins.throw "The Haskell package set does not contain the package: serialise (build dependency)"))
            ];
          };
        };
      tests = {
        "hnix-tests" = {
          depends = ([
            (hsPkgs."Diff" or (builtins.throw "The Haskell package set does not contain the package: Diff (build dependency)"))
            (hsPkgs."Glob" or (builtins.throw "The Haskell package set does not contain the package: Glob (build dependency)"))
            (hsPkgs."base" or (builtins.throw "The Haskell package set does not contain the package: base (build dependency)"))
            (hsPkgs."bytestring" or (builtins.throw "The Haskell package set does not contain the package: bytestring (build dependency)"))
            (hsPkgs."containers" or (builtins.throw "The Haskell package set does not contain the package: containers (build dependency)"))
            (hsPkgs."data-fix" or (builtins.throw "The Haskell package set does not contain the package: data-fix (build dependency)"))
            (hsPkgs."deepseq" or (builtins.throw "The Haskell package set does not contain the package: deepseq (build dependency)"))
            (hsPkgs."dependent-sum" or (builtins.throw "The Haskell package set does not contain the package: dependent-sum (build dependency)"))
            (hsPkgs."directory" or (builtins.throw "The Haskell package set does not contain the package: directory (build dependency)"))
            (hsPkgs."exceptions" or (builtins.throw "The Haskell package set does not contain the package: exceptions (build dependency)"))
            (hsPkgs."filepath" or (builtins.throw "The Haskell package set does not contain the package: filepath (build dependency)"))
            (hsPkgs."generic-random" or (builtins.throw "The Haskell package set does not contain the package: generic-random (build dependency)"))
            (hsPkgs."hashing" or (builtins.throw "The Haskell package set does not contain the package: hashing (build dependency)"))
            (hsPkgs."hedgehog" or (builtins.throw "The Haskell package set does not contain the package: hedgehog (build dependency)"))
            (hsPkgs."hnix" or (builtins.throw "The Haskell package set does not contain the package: hnix (build dependency)"))
            (hsPkgs."interpolate" or (builtins.throw "The Haskell package set does not contain the package: interpolate (build dependency)"))
            (hsPkgs."megaparsec" or (builtins.throw "The Haskell package set does not contain the package: megaparsec (build dependency)"))
            (hsPkgs."mtl" or (builtins.throw "The Haskell package set does not contain the package: mtl (build dependency)"))
            (hsPkgs."optparse-applicative" or (builtins.throw "The Haskell package set does not contain the package: optparse-applicative (build dependency)"))
            (hsPkgs."pretty-show" or (builtins.throw "The Haskell package set does not contain the package: pretty-show (build dependency)"))
            (hsPkgs."prettyprinter" or (builtins.throw "The Haskell package set does not contain the package: prettyprinter (build dependency)"))
            (hsPkgs."process" or (builtins.throw "The Haskell package set does not contain the package: process (build dependency)"))
            (hsPkgs."split" or (builtins.throw "The Haskell package set does not contain the package: split (build dependency)"))
            (hsPkgs."tasty" or (builtins.throw "The Haskell package set does not contain the package: tasty (build dependency)"))
            (hsPkgs."tasty-hedgehog" or (builtins.throw "The Haskell package set does not contain the package: tasty-hedgehog (build dependency)"))
            (hsPkgs."tasty-hunit" or (builtins.throw "The Haskell package set does not contain the package: tasty-hunit (build dependency)"))
            (hsPkgs."tasty-quickcheck" or (builtins.throw "The Haskell package set does not contain the package: tasty-quickcheck (build dependency)"))
            (hsPkgs."tasty-th" or (builtins.throw "The Haskell package set does not contain the package: tasty-th (build dependency)"))
            (hsPkgs."template-haskell" or (builtins.throw "The Haskell package set does not contain the package: template-haskell (build dependency)"))
            (hsPkgs."text" or (builtins.throw "The Haskell package set does not contain the package: text (build dependency)"))
            (hsPkgs."time" or (builtins.throw "The Haskell package set does not contain the package: time (build dependency)"))
            (hsPkgs."transformers" or (builtins.throw "The Haskell package set does not contain the package: transformers (build dependency)"))
            (hsPkgs."unix" or (builtins.throw "The Haskell package set does not contain the package: unix (build dependency)"))
            (hsPkgs."unordered-containers" or (builtins.throw "The Haskell package set does not contain the package: unordered-containers (build dependency)"))
            ] ++ (pkgs.lib).optional (system.isLinux && (compiler.isGhc && (compiler.version).ge "8.2") && (compiler.isGhc && (compiler.version).lt "8.3")) (hsPkgs."compact" or (builtins.throw "The Haskell package set does not contain the package: compact (build dependency)"))) ++ (pkgs.lib).optionals (!(compiler.isGhcjs && true)) [
            (hsPkgs."base16-bytestring" or (builtins.throw "The Haskell package set does not contain the package: base16-bytestring (build dependency)"))
            (hsPkgs."cryptohash-md5" or (builtins.throw "The Haskell package set does not contain the package: cryptohash-md5 (build dependency)"))
            (hsPkgs."cryptohash-sha1" or (builtins.throw "The Haskell package set does not contain the package: cryptohash-sha1 (build dependency)"))
            (hsPkgs."cryptohash-sha256" or (builtins.throw "The Haskell package set does not contain the package: cryptohash-sha256 (build dependency)"))
            (hsPkgs."cryptohash-sha512" or (builtins.throw "The Haskell package set does not contain the package: cryptohash-sha512 (build dependency)"))
            (hsPkgs."serialise" or (builtins.throw "The Haskell package set does not contain the package: serialise (build dependency)"))
            ];
          build-tools = [
            (hsPkgs.buildPackages.hspec-discover or (pkgs.buildPackages.hspec-discover or (builtins.throw "Neither the Haskell package set or the Nixpkgs package set contain the package: hspec-discover (build tool dependency)")))
            ];
          };
        };
      benchmarks = {
        "hnix-benchmarks" = {
          depends = ([
            (hsPkgs."base" or (builtins.throw "The Haskell package set does not contain the package: base (build dependency)"))
            (hsPkgs."bytestring" or (builtins.throw "The Haskell package set does not contain the package: bytestring (build dependency)"))
            (hsPkgs."containers" or (builtins.throw "The Haskell package set does not contain the package: containers (build dependency)"))
            (hsPkgs."criterion" or (builtins.throw "The Haskell package set does not contain the package: criterion (build dependency)"))
            (hsPkgs."data-fix" or (builtins.throw "The Haskell package set does not contain the package: data-fix (build dependency)"))
            (hsPkgs."deepseq" or (builtins.throw "The Haskell package set does not contain the package: deepseq (build dependency)"))
            (hsPkgs."exceptions" or (builtins.throw "The Haskell package set does not contain the package: exceptions (build dependency)"))
            (hsPkgs."filepath" or (builtins.throw "The Haskell package set does not contain the package: filepath (build dependency)"))
            (hsPkgs."hashing" or (builtins.throw "The Haskell package set does not contain the package: hashing (build dependency)"))
            (hsPkgs."hnix" or (builtins.throw "The Haskell package set does not contain the package: hnix (build dependency)"))
            (hsPkgs."mtl" or (builtins.throw "The Haskell package set does not contain the package: mtl (build dependency)"))
            (hsPkgs."optparse-applicative" or (builtins.throw "The Haskell package set does not contain the package: optparse-applicative (build dependency)"))
            (hsPkgs."template-haskell" or (builtins.throw "The Haskell package set does not contain the package: template-haskell (build dependency)"))
            (hsPkgs."text" or (builtins.throw "The Haskell package set does not contain the package: text (build dependency)"))
            (hsPkgs."time" or (builtins.throw "The Haskell package set does not contain the package: time (build dependency)"))
            (hsPkgs."transformers" or (builtins.throw "The Haskell package set does not contain the package: transformers (build dependency)"))
            (hsPkgs."unordered-containers" or (builtins.throw "The Haskell package set does not contain the package: unordered-containers (build dependency)"))
            ] ++ (pkgs.lib).optional (system.isLinux && (compiler.isGhc && (compiler.version).ge "8.2") && (compiler.isGhc && (compiler.version).lt "8.3")) (hsPkgs."compact" or (builtins.throw "The Haskell package set does not contain the package: compact (build dependency)"))) ++ (pkgs.lib).optionals (!(compiler.isGhcjs && true)) [
            (hsPkgs."base16-bytestring" or (builtins.throw "The Haskell package set does not contain the package: base16-bytestring (build dependency)"))
            (hsPkgs."cryptohash-md5" or (builtins.throw "The Haskell package set does not contain the package: cryptohash-md5 (build dependency)"))
            (hsPkgs."cryptohash-sha1" or (builtins.throw "The Haskell package set does not contain the package: cryptohash-sha1 (build dependency)"))
            (hsPkgs."cryptohash-sha256" or (builtins.throw "The Haskell package set does not contain the package: cryptohash-sha256 (build dependency)"))
            (hsPkgs."cryptohash-sha512" or (builtins.throw "The Haskell package set does not contain the package: cryptohash-sha512 (build dependency)"))
            (hsPkgs."serialise" or (builtins.throw "The Haskell package set does not contain the package: serialise (build dependency)"))
            ];
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/haskell-nix/hnix.git";
      rev = "617d0867ab96c8f97b02c4524bd948d9f114005e";
      sha256 = "037kxj9wirynmavlp7d0k19a5xrhj117hlh3yia12xj6v828b99z";
      });
    }