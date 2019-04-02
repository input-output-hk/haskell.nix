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
          (hsPkgs.base)
          (hsPkgs.hnix)
          (hsPkgs.aeson)
          (hsPkgs.unordered-containers)
          (hsPkgs.process)
          (hsPkgs.deepseq)
          (hsPkgs.transformers)
          (hsPkgs.data-fix)
          (hsPkgs.Cabal)
          (hsPkgs.text)
          (hsPkgs.filepath)
          (hsPkgs.directory)
          (hsPkgs.bytestring)
          (hsPkgs.cryptohash-sha256)
          (hsPkgs.base16-bytestring)
          (hsPkgs.hpack)
          ];
        };
      exes = {
        "cabal-to-nix" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.transformers)
            (hsPkgs.bytestring)
            (hsPkgs.hpack)
            (hsPkgs.hnix)
            (hsPkgs.text)
            (hsPkgs.nix-tools)
            (hsPkgs.filepath)
            (hsPkgs.directory)
            (hsPkgs.prettyprinter)
            ];
          };
        "hashes-to-nix" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.hnix)
            (hsPkgs.nix-tools)
            (hsPkgs.data-fix)
            (hsPkgs.aeson)
            (hsPkgs.microlens)
            (hsPkgs.microlens-aeson)
            (hsPkgs.text)
            (hsPkgs.filepath)
            (hsPkgs.directory)
            ];
          };
        "plan-to-nix" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.nix-tools)
            (hsPkgs.hnix)
            (hsPkgs.Cabal)
            (hsPkgs.text)
            (hsPkgs.hpack)
            (hsPkgs.unordered-containers)
            (hsPkgs.vector)
            (hsPkgs.aeson)
            (hsPkgs.microlens)
            (hsPkgs.microlens-aeson)
            (hsPkgs.optparse-applicative)
            (hsPkgs.prettyprinter)
            (hsPkgs.filepath)
            (hsPkgs.directory)
            (hsPkgs.bytestring)
            (hsPkgs.transformers)
            (hsPkgs.extra)
            ];
          };
        "hackage-to-nix" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.nix-tools)
            (hsPkgs.hackage-db)
            (hsPkgs.hnix)
            (hsPkgs.Cabal)
            (hsPkgs.containers)
            (hsPkgs.bytestring)
            (hsPkgs.text)
            (hsPkgs.cryptohash-sha256)
            (hsPkgs.base16-bytestring)
            (hsPkgs.filepath)
            (hsPkgs.directory)
            (hsPkgs.transformers)
            ];
          };
        "lts-to-nix" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.nix-tools)
            (hsPkgs.hnix)
            (hsPkgs.yaml)
            (hsPkgs.aeson)
            (hsPkgs.microlens)
            (hsPkgs.microlens-aeson)
            (hsPkgs.text)
            (hsPkgs.filepath)
            (hsPkgs.directory)
            (hsPkgs.unordered-containers)
            (hsPkgs.Cabal)
            ];
          };
        "stack-to-nix" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.nix-tools)
            (hsPkgs.transformers)
            (hsPkgs.hnix)
            (hsPkgs.yaml)
            (hsPkgs.aeson)
            (hsPkgs.microlens)
            (hsPkgs.microlens-aeson)
            (hsPkgs.text)
            (hsPkgs.Cabal)
            (hsPkgs.vector)
            (hsPkgs.prettyprinter)
            (hsPkgs.directory)
            (hsPkgs.filepath)
            (hsPkgs.extra)
            (hsPkgs.hpack)
            (hsPkgs.bytestring)
            (hsPkgs.optparse-applicative)
            (hsPkgs.http-client-tls)
            (hsPkgs.http-client)
            (hsPkgs.http-types)
            ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault .././../.; }