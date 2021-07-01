{ system
  , compiler
  , flags
  , pkgs
  , hsPkgs
  , pkgconfPkgs
  , errorHandler
  , config
  , ... }:
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
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" ];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [ "ChangeLog.md" ];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."Cabal" or (errorHandler.buildDepError "Cabal"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."aeson-pretty" or (errorHandler.buildDepError "aeson-pretty"))
          (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cryptohash-sha256" or (errorHandler.buildDepError "cryptohash-sha256"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."data-fix" or (errorHandler.buildDepError "data-fix"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."hnix" or (errorHandler.buildDepError "hnix"))
          (hsPkgs."hpack" or (errorHandler.buildDepError "hpack"))
          (hsPkgs."http-client" or (errorHandler.buildDepError "http-client"))
          (hsPkgs."http-client-tls" or (errorHandler.buildDepError "http-client-tls"))
          (hsPkgs."http-types" or (errorHandler.buildDepError "http-types"))
          (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
          (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
          (hsPkgs."process" or (errorHandler.buildDepError "process"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          (hsPkgs."yaml" or (errorHandler.buildDepError "yaml"))
          ];
        buildable = true;
        modules = [
          "Cabal2Nix"
          "Cabal2Nix/Util"
          "Cabal2Nix/Plan"
          "CabalName"
          "CabalName/CLI"
          "Distribution/Nixpkgs/Fetch"
          "StackRepos"
          "StackRepos/CLI"
          "Stack2nix"
          "Stack2nix/Cache"
          "Stack2nix/CLI"
          "Stack2nix/External/Resolve"
          "Stack2nix/Project"
          "Stack2nix/Stack"
          ];
        hsSourceDirs = [ "lib" ];
        };
      exes = {
        "cabal-to-nix" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."hpack" or (errorHandler.buildDepError "hpack"))
            (hsPkgs."hnix" or (errorHandler.buildDepError "hnix"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."nix-tools" or (errorHandler.buildDepError "nix-tools"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
            ];
          buildable = true;
          hsSourceDirs = [ "cabal2nix" ];
          mainPath = [ "Main.hs" ];
          };
        "hashes-to-nix" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."hnix" or (errorHandler.buildDepError "hnix"))
            (hsPkgs."nix-tools" or (errorHandler.buildDepError "nix-tools"))
            (hsPkgs."data-fix" or (errorHandler.buildDepError "data-fix"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
            (hsPkgs."microlens-aeson" or (errorHandler.buildDepError "microlens-aeson"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            ];
          buildable = true;
          hsSourceDirs = [ "hashes2nix" ];
          mainPath = [ "Main.hs" ];
          };
        "plan-to-nix" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."nix-tools" or (errorHandler.buildDepError "nix-tools"))
            (hsPkgs."hnix" or (errorHandler.buildDepError "hnix"))
            (hsPkgs."Cabal" or (errorHandler.buildDepError "Cabal"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."hpack" or (errorHandler.buildDepError "hpack"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
            (hsPkgs."microlens-aeson" or (errorHandler.buildDepError "microlens-aeson"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
            ];
          buildable = true;
          modules = [
            "Plan2Nix"
            "Plan2Nix/Cache"
            "Plan2Nix/CLI"
            "Plan2Nix/Project"
            "Plan2Nix/Plan"
            ];
          hsSourceDirs = [ "plan2nix" ];
          mainPath = [ "Main.hs" ];
          };
        "hackage-to-nix" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."nix-tools" or (errorHandler.buildDepError "nix-tools"))
            (hsPkgs."hackage-db" or (errorHandler.buildDepError "hackage-db"))
            (hsPkgs."hnix" or (errorHandler.buildDepError "hnix"))
            (hsPkgs."Cabal" or (errorHandler.buildDepError "Cabal"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."aeson-pretty" or (errorHandler.buildDepError "aeson-pretty"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."cryptohash-sha256" or (errorHandler.buildDepError "cryptohash-sha256"))
            (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
            (hsPkgs."base64-bytestring" or (errorHandler.buildDepError "base64-bytestring"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            ];
          buildable = true;
          hsSourceDirs = [ "hackage2nix" ];
          mainPath = [ "Main.hs" ];
          };
        "lts-to-nix" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."nix-tools" or (errorHandler.buildDepError "nix-tools"))
            (hsPkgs."hnix" or (errorHandler.buildDepError "hnix"))
            (hsPkgs."yaml" or (errorHandler.buildDepError "yaml"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
            (hsPkgs."microlens-aeson" or (errorHandler.buildDepError "microlens-aeson"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."Cabal" or (errorHandler.buildDepError "Cabal"))
            ];
          buildable = true;
          hsSourceDirs = [ "lts2nix" ];
          mainPath = [ "Main.hs" ];
          };
        "stack-to-nix" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."nix-tools" or (errorHandler.buildDepError "nix-tools"))
            ];
          buildable = true;
          hsSourceDirs = [ "stack2nix" ];
          mainPath = [ "Main.hs" ];
          };
        "truncate-index" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."zlib" or (errorHandler.buildDepError "zlib"))
            (hsPkgs."tar" or (errorHandler.buildDepError "tar"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            ];
          buildable = true;
          hsSourceDirs = [ "truncate-index" ];
          mainPath = [ "Main.hs" ];
          };
        "stack-repos" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."nix-tools" or (errorHandler.buildDepError "nix-tools"))
            ];
          buildable = true;
          hsSourceDirs = [ "stack-repos" ];
          mainPath = [ "Main.hs" ];
          };
        "cabal-name" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."nix-tools" or (errorHandler.buildDepError "nix-tools"))
            ];
          buildable = true;
          hsSourceDirs = [ "cabal-name" ];
          mainPath = [ "Main.hs" ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../.; }