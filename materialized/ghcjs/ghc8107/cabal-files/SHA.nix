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
    flags = { exe = false; };
    package = {
      specVersion = "1.8";
      identifier = { name = "SHA"; version = "1.6.4.4"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Adam Wick <awick@galois.com>,\nRaphael Javaux <raphaeljavaux@gmail.com>";
      author = "Adam Wick <awick@galois.com>, Brian Lewis <brian@lorf.org>";
      homepage = "";
      url = "";
      synopsis = "Implementations of the SHA suite of message digest functions";
      description = "This library implements the SHA suite of message digest functions,\naccording to NIST FIPS 180-2 (with the SHA-224 addendum), as well\nas the SHA-based HMAC routines. The functions have been tested\nagainst most of the NIST and RFC test vectors for the various\nfunctions. While some attention has been paid to performance,\nthese do not presently reach the speed of well-tuned libraries,\nlike OpenSSL.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          ];
        buildable = true;
        };
      exes = {
        "sha1" = {
          depends = (pkgs.lib).optionals (flags.exe) [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."SHA" or (errorHandler.buildDepError "SHA"))
            ];
          buildable = if flags.exe then true else false;
          };
        "sha224" = {
          depends = (pkgs.lib).optionals (flags.exe) [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."SHA" or (errorHandler.buildDepError "SHA"))
            ];
          buildable = if flags.exe then true else false;
          };
        "sha256" = {
          depends = (pkgs.lib).optionals (flags.exe) [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."SHA" or (errorHandler.buildDepError "SHA"))
            ];
          buildable = if flags.exe then true else false;
          };
        "sha384" = {
          depends = (pkgs.lib).optionals (flags.exe) [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."SHA" or (errorHandler.buildDepError "SHA"))
            ];
          buildable = if flags.exe then true else false;
          };
        "sha512" = {
          depends = (pkgs.lib).optionals (flags.exe) [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."SHA" or (errorHandler.buildDepError "SHA"))
            ];
          buildable = if flags.exe then true else false;
          };
        };
      tests = {
        "test-sha" = {
          depends = [
            (hsPkgs."array" or (errorHandler.buildDepError "array"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."test-framework" or (errorHandler.buildDepError "test-framework"))
            (hsPkgs."test-framework-quickcheck2" or (errorHandler.buildDepError "test-framework-quickcheck2"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/SHA-1.6.4.4.tar.gz";
      sha256 = "6bd950df6b11a3998bb1452d875d2da043ee43385459afc5f16d471d25178b44";
      });
    }