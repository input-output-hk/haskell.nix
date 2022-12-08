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
    flags = { io-testsuite = false; };
    package = {
      specVersion = "2.2";
      identifier = { name = "hnix-store-remote"; version = "0.5.0.0"; };
      license = "Apache-2.0";
      copyright = "2018 Richard Marko";
      maintainer = "srk@48.io";
      author = "Richard Marko";
      homepage = "https://github.com/haskell-nix/hnix-store";
      url = "";
      synopsis = "Remote hnix store";
      description = "Implementation of the nix store using the daemon protocol.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."attoparsec" or (errorHandler.buildDepError "attoparsec"))
          (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."cryptonite" or (errorHandler.buildDepError "cryptonite"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."network" or (errorHandler.buildDepError "network"))
          (hsPkgs."nix-derivation" or (errorHandler.buildDepError "nix-derivation"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          (hsPkgs."hnix-store-core" or (errorHandler.buildDepError "hnix-store-core"))
          ];
        buildable = true;
        };
      tests = {
        "hnix-store-remote-tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."hnix-store-core" or (errorHandler.buildDepError "hnix-store-core"))
            (hsPkgs."hnix-store-remote" or (errorHandler.buildDepError "hnix-store-remote"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."cryptonite" or (errorHandler.buildDepError "cryptonite"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."process" or (errorHandler.buildDepError "process"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."hspec-expectations-lifted" or (errorHandler.buildDepError "hspec-expectations-lifted"))
            (hsPkgs."quickcheck-text" or (errorHandler.buildDepError "quickcheck-text"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."tasty-hspec" or (errorHandler.buildDepError "tasty-hspec"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."linux-namespaces" or (errorHandler.buildDepError "linux-namespaces"))
            (hsPkgs."nix-derivation" or (errorHandler.buildDepError "nix-derivation"))
            (hsPkgs."temporary" or (errorHandler.buildDepError "temporary"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."unix" or (errorHandler.buildDepError "unix"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            ];
          build-tools = [
            (hsPkgs.buildPackages.tasty-discover.components.exes.tasty-discover or (pkgs.buildPackages.tasty-discover or (errorHandler.buildToolDepError "tasty-discover:tasty-discover")))
            ];
          buildable = if !flags.io-testsuite then false else true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/hnix-store-remote-0.5.0.0.tar.gz";
      sha256 = "c94d112e649de501f31b8e8956d8e3b89cddeff8db98e2702282458268887877";
      });
    }