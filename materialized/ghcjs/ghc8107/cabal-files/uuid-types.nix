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
      specVersion = "1.12";
      identifier = { name = "uuid-types"; version = "1.0.4"; };
      license = "BSD-3-Clause";
      copyright = "(c) 2017-2018 Herbert Valerio Riedel\n(c) 2008-2014 Antoine Latter";
      maintainer = "Oleg Grenrus <oleg.grenrus@iki.fi>";
      author = "Antoine Latter";
      homepage = "https://github.com/haskell-hvr/uuid";
      url = "";
      synopsis = "Type definitions for Universally Unique Identifiers";
      description = "This library contains type definitions for\n<https://en.wikipedia.org/wiki/UUID Universally Unique Identifiers (UUID)>\n(as specified in\n<http://tools.ietf.org/html/rfc4122 RFC 4122>)\nand basic conversion functions.\n\nSee also the <https://hackage.haskell.org/package/uuid 'uuid' package>\nproviding a high-level API for managing the different UUID versions.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."random" or (errorHandler.buildDepError "random"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          ];
        buildable = true;
        };
      tests = {
        "testuuid" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."uuid-types" or (errorHandler.buildDepError "uuid-types"))
            (hsPkgs."ghc-byteorder" or (errorHandler.buildDepError "ghc-byteorder"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/uuid-types-1.0.4.tar.gz";
      sha256 = "c2aa2ccaa3a74259aca1f57cc1c277822086430814ce5e4f38cfd868fe48ec06";
      });
    }