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
      specVersion = "2.0";
      identifier = { name = "saltine"; version = "0.2.0.1"; };
      license = "MIT";
      copyright = "Copyright (c) Joseph Abrahamson 2013";
      maintainer = "Max Amanshauser <max@lambdalifting.org>";
      author = "Joseph Abrahamson";
      homepage = "";
      url = "";
      synopsis = "Cryptography that's easy to digest (NaCl/libsodium bindings).";
      description = "/NaCl/ (pronounced \\\"salt\\\") is a new easy-to-use high-speed software\nlibrary for network communication, encryption, decryption,\nsignatures, etc. NaCl's goal is to provide all of the core\noperations needed to build higher-level cryptographic tools.\n\n<http://nacl.cr.yp.to/>\n\n/Sodium/ is a portable, cross-compilable, installable, packageable\ncrypto library based on NaCl, with a compatible API.\n\n<https://github.com/jedisct1/libsodium>\n\n/Saltine/ is a Haskell binding to the NaCl primitives going through\nSodium for build convenience and, eventually, portability.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."profunctors" or (errorHandler.buildDepError "profunctors"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          ];
        libs = (pkgs.lib).optional (system.isWindows) (pkgs."sodium" or (errorHandler.sysDepError "sodium"));
        pkgconfig = (pkgs.lib).optional (!system.isWindows) (pkgconfPkgs."libsodium" or (errorHandler.pkgConfDepError "libsodium"));
        buildable = true;
        };
      tests = {
        "tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."saltine" or (errorHandler.buildDepError "saltine"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."test-framework-quickcheck2" or (errorHandler.buildDepError "test-framework-quickcheck2"))
            (hsPkgs."test-framework" or (errorHandler.buildDepError "test-framework"))
            (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
            ];
          buildable = true;
          };
        };
      benchmarks = {
        "benchmarks" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."saltine" or (errorHandler.buildDepError "saltine"))
            ];
          libs = [ (pkgs."sodium" or (errorHandler.sysDepError "sodium")) ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/saltine-0.2.0.1.tar.gz";
      sha256 = "ce6867a08a308bc5155cb4c5a146837cf6c5e056dd0d0151f9f0838b2eed2ea0";
      });
    }