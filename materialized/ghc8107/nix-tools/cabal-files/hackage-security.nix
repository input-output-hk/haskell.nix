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
    flags = {
      base48 = true;
      use-network-uri = true;
      cabal-syntax = false;
      old-directory = false;
      mtl21 = false;
      lukko = true;
      };
    package = {
      specVersion = "1.12";
      identifier = { name = "hackage-security"; version = "0.6.2.2"; };
      license = "BSD-3-Clause";
      copyright = "Copyright 2015-2022 Well-Typed LLP";
      maintainer = "cabal-devel@haskell.org";
      author = "Edsko de Vries";
      homepage = "https://github.com/haskell/hackage-security";
      url = "";
      synopsis = "Hackage security library";
      description = "The hackage security library provides both server and\nclient utilities for securing the Hackage package server\n(<https://hackage.haskell.org/>).  It is based on The Update\nFramework (<https://theupdateframework.com/>), a set of\nrecommendations developed by security researchers at\nvarious universities in the US as well as developers on the\nTor project (<https://www.torproject.org/>).\n\nThe current implementation supports only index signing,\nthereby enabling untrusted mirrors. It does not yet provide\nfacilities for author package signing.\n\nThe library has two main entry points:\n\"Hackage.Security.Client\" is the main entry point for\nclients (the typical example being @cabal@), and\n\"Hackage.Security.Server\" is the main entry point for\nservers (the typical example being @hackage-server@).";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = ((((([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
          (hsPkgs."base64-bytestring" or (errorHandler.buildDepError "base64-bytestring"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."ed25519" or (errorHandler.buildDepError "ed25519"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."parsec" or (errorHandler.buildDepError "parsec"))
          (hsPkgs."pretty" or (errorHandler.buildDepError "pretty"))
          (hsPkgs."cryptohash-sha256" or (errorHandler.buildDepError "cryptohash-sha256"))
          (hsPkgs."tar" or (errorHandler.buildDepError "tar"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."zlib" or (errorHandler.buildDepError "zlib"))
          (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
          ] ++ (if flags.old-directory
          then [
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."old-time" or (errorHandler.buildDepError "old-time"))
            ]
          else [
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            ])) ++ (if flags.mtl21
          then [
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."mtl-compat" or (errorHandler.buildDepError "mtl-compat"))
            ]
          else [
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            ])) ++ (if flags.lukko
          then [ (hsPkgs."lukko" or (errorHandler.buildDepError "lukko")) ]
          else [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            ])) ++ (if flags.cabal-syntax && (compiler.isGhc && (compiler.version).ge "8.2")
          then [
            (hsPkgs."Cabal-syntax" or (errorHandler.buildDepError "Cabal-syntax"))
            ]
          else [
            (hsPkgs."Cabal" or (errorHandler.buildDepError "Cabal"))
            (hsPkgs."Cabal-syntax" or (errorHandler.buildDepError "Cabal-syntax"))
            ])) ++ (if flags.base48
          then [ (hsPkgs."base" or (errorHandler.buildDepError "base")) ]
          else [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."old-locale" or (errorHandler.buildDepError "old-locale"))
            ])) ++ (if flags.use-network-uri
          then [
            (hsPkgs."network-uri" or (errorHandler.buildDepError "network-uri"))
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            ]
          else [
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            ]);
        buildable = true;
        };
      tests = {
        "TestSuite" = {
          depends = [
            (hsPkgs."hackage-security" or (errorHandler.buildDepError "hackage-security"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."network-uri" or (errorHandler.buildDepError "network-uri"))
            (hsPkgs."tar" or (errorHandler.buildDepError "tar"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."zlib" or (errorHandler.buildDepError "zlib"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            (hsPkgs."temporary" or (errorHandler.buildDepError "temporary"))
            ] ++ [
            (hsPkgs."Cabal" or (errorHandler.buildDepError "Cabal"))
            (hsPkgs."Cabal-syntax" or (errorHandler.buildDepError "Cabal-syntax"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/hackage-security-0.6.2.2.tar.gz";
      sha256 = "a833a31610220506557e96fb6e3e8b05a2e1db8e7ebc06e91bbb699ddde0b276";
      });
    }