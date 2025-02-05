{ system
  , compiler
  , flags
  , pkgs
  , hsPkgs
  , pkgconfPkgs
  , errorHandler
  , config
  , ... }:
  ({
    flags = { cabal-syntax = false; lukko = true; };
    package = {
      specVersion = "1.12";
      identifier = { name = "hackage-security"; version = "0.6.2.6"; };
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
        depends = ([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
          (hsPkgs."base64-bytestring" or (errorHandler.buildDepError "base64-bytestring"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."cryptohash-sha256" or (errorHandler.buildDepError "cryptohash-sha256"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."ed25519" or (errorHandler.buildDepError "ed25519"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."network-uri" or (errorHandler.buildDepError "network-uri"))
          (hsPkgs."network" or (errorHandler.buildDepError "network"))
          (hsPkgs."parsec" or (errorHandler.buildDepError "parsec"))
          (hsPkgs."pretty" or (errorHandler.buildDepError "pretty"))
          (hsPkgs."tar" or (errorHandler.buildDepError "tar"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."zlib" or (errorHandler.buildDepError "zlib"))
          (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
        ] ++ (if flags.lukko
          then [ (hsPkgs."lukko" or (errorHandler.buildDepError "lukko")) ]
          else [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
          ])) ++ (if flags.cabal-syntax
          then [
            (hsPkgs."Cabal-syntax" or (errorHandler.buildDepError "Cabal-syntax"))
          ]
          else [
            (hsPkgs."Cabal" or (errorHandler.buildDepError "Cabal"))
            (hsPkgs."Cabal-syntax" or (errorHandler.buildDepError "Cabal-syntax"))
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
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/hackage-security-0.6.2.6.tar.gz";
      sha256 = "2e4261576b3e11b9f5175392947f56a638cc1a3584b8acbb962b809d7c69db69";
    });
  }) // {
    package-description-override = "cabal-version:       1.12\r\nname:                hackage-security\r\nversion:             0.6.2.6\r\nx-revision: 4\r\n\r\nsynopsis:            Hackage security library\r\ndescription:         The hackage security library provides both server and\r\n                     client utilities for securing the Hackage package server\r\n                     (<https://hackage.haskell.org/>).  It is based on The Update\r\n                     Framework (<https://theupdateframework.com/>), a set of\r\n                     recommendations developed by security researchers at\r\n                     various universities in the US as well as developers on the\r\n                     Tor project (<https://www.torproject.org/>).\r\n                     .\r\n                     The current implementation supports only index signing,\r\n                     thereby enabling untrusted mirrors. It does not yet provide\r\n                     facilities for author package signing.\r\n                     .\r\n                     The library has two main entry points:\r\n                     \"Hackage.Security.Client\" is the main entry point for\r\n                     clients (the typical example being @cabal@), and\r\n                     \"Hackage.Security.Server\" is the main entry point for\r\n                     servers (the typical example being @hackage-server@).\r\nlicense:             BSD3\r\nlicense-file:        LICENSE\r\nauthor:              Edsko de Vries\r\nmaintainer:          cabal-devel@haskell.org\r\ncopyright:           Copyright 2015-2022 Well-Typed LLP\r\ncategory:            Distribution\r\nhomepage:            https://github.com/haskell/hackage-security\r\nbug-reports:         https://github.com/haskell/hackage-security/issues\r\nbuild-type:          Simple\r\n\r\ntested-with:\r\n  GHC == 9.10.1\r\n  GHC == 9.8.2\r\n  GHC == 9.6.5\r\n  GHC == 9.4.8\r\n  GHC == 9.2.8\r\n  GHC == 9.0.2\r\n  GHC == 8.10.7\r\n  GHC == 8.8.4\r\n  GHC == 8.6.5\r\n  GHC == 8.4.4\r\n\r\nextra-source-files:\r\n  ChangeLog.md\r\n\r\nsource-repository head\r\n  type: git\r\n  location: https://github.com/haskell/hackage-security.git\r\n\r\nflag Cabal-syntax\r\n  description: Are we using Cabal-syntax?\r\n  manual: False\r\n  default: False\r\n\r\nflag lukko\r\n  description: Use @lukko@ for file-locking, otherwise use @GHC.IO.Handle.Lock@\r\n  manual:      True\r\n  default:     True\r\n\r\nlibrary\r\n  -- Most functionality is exported through the top-level entry points .Client\r\n  -- and .Server; the other exported modules are intended for qualified imports.\r\n  exposed-modules:     Hackage.Security.Client\r\n                       Hackage.Security.Client.Formats\r\n                       Hackage.Security.Client.Repository\r\n                       Hackage.Security.Client.Repository.Cache\r\n                       Hackage.Security.Client.Repository.Local\r\n                       Hackage.Security.Client.Repository.Remote\r\n                       Hackage.Security.Client.Repository.HttpLib\r\n                       Hackage.Security.Client.Verify\r\n                       Hackage.Security.JSON\r\n                       Hackage.Security.Key.Env\r\n                       Hackage.Security.Server\r\n                       Hackage.Security.Trusted\r\n                       Hackage.Security.TUF.FileMap\r\n                       Hackage.Security.Util.Checked\r\n                       Hackage.Security.Util.Path\r\n                       Hackage.Security.Util.Pretty\r\n                       Hackage.Security.Util.Some\r\n                       Text.JSON.Canonical\r\n  other-modules:       Hackage.Security.Key\r\n                       Hackage.Security.Trusted.TCB\r\n                       Hackage.Security.TUF\r\n                       Hackage.Security.TUF.Common\r\n                       Hackage.Security.TUF.FileInfo\r\n                       Hackage.Security.TUF.Header\r\n                       Hackage.Security.TUF.Layout.Cache\r\n                       Hackage.Security.TUF.Layout.Index\r\n                       Hackage.Security.TUF.Layout.Repo\r\n                       Hackage.Security.TUF.Mirrors\r\n                       Hackage.Security.TUF.Paths\r\n                       Hackage.Security.TUF.Patterns\r\n                       Hackage.Security.TUF.Root\r\n                       Hackage.Security.TUF.Signed\r\n                       Hackage.Security.TUF.Snapshot\r\n                       Hackage.Security.TUF.Targets\r\n                       Hackage.Security.TUF.Timestamp\r\n                       Hackage.Security.Util.Base64\r\n                       Hackage.Security.Util.Exit\r\n                       Hackage.Security.Util.IO\r\n                       Hackage.Security.Util.JSON\r\n                       Hackage.Security.Util.Lens\r\n                       Hackage.Security.Util.Stack\r\n                       Hackage.Security.Util.TypedEmbedded\r\n\r\n  build-depends:       base              >= 4.11     && < 4.21,\r\n                       base16-bytestring >= 0.1.1    && < 1.1,\r\n                       base64-bytestring >= 1.0      && < 1.3,\r\n                       bytestring        >= 0.10.8.2 && < 0.13,\r\n                       containers        >= 0.5.11   && < 0.8,\r\n                       cryptohash-sha256 >= 0.11     && < 0.12,\r\n                       directory         >= 1.3.1.5  && < 1.4,\r\n                       ed25519           >= 0.0      && < 0.1,\r\n                       filepath          >= 1.4.2    && < 1.6,\r\n                       mtl               >= 2.2.2    && < 2.4,\r\n                       network-uri       >= 2.6      && < 2.7,\r\n                       network           >= 2.6      && < 3.3,\r\n                       parsec            >= 3.1.13   && < 3.2,\r\n                       pretty            >= 1.0      && < 1.2,\r\n                       -- 0.4.2 introduces TarIndex, 0.4.4 introduces more\r\n                       -- functionality, 0.5.0 changes type of serialise\r\n                       tar               >= 0.5      && < 0.7,\r\n                       template-haskell  >= 2.13     && < 2.23,\r\n                       time              >= 1.8.0.2  && < 1.15,\r\n                       transformers      >= 0.3      && < 0.7,\r\n                       zlib              >= 0.5      && < 0.8,\r\n                       -- whatever versions are bundled with ghc:\r\n                       ghc-prim          >= 0.5.2    && < 0.12\r\n\r\n  if flag(lukko)\r\n    build-depends:     lukko      >= 0.1     && < 0.2\r\n  else\r\n    build-depends:     base       >= 4.11\r\n\r\n  if flag(Cabal-syntax)\r\n    build-depends: Cabal-syntax >= 3.7 && < 3.16\r\n  else\r\n    build-depends: Cabal        >= 2.2.0.1 && < 2.6\r\n                             || >= 3.0     && < 3.7,\r\n                   Cabal-syntax <  3.7\r\n\r\n  hs-source-dirs:      src\r\n  default-language:    Haskell2010\r\n  default-extensions:  DefaultSignatures\r\n                       DeriveDataTypeable\r\n                       DeriveFunctor\r\n                       FlexibleContexts\r\n                       FlexibleInstances\r\n                       GADTs\r\n                       GeneralizedNewtypeDeriving\r\n                       KindSignatures\r\n                       MultiParamTypeClasses\r\n                       NamedFieldPuns\r\n                       NoImplicitPrelude\r\n                       NoMonomorphismRestriction\r\n                       PatternSynonyms\r\n                       RankNTypes\r\n                       RecordWildCards\r\n                       ScopedTypeVariables\r\n                       StandaloneDeriving\r\n                       TupleSections\r\n                       TypeFamilies\r\n                       TypeOperators\r\n                       ViewPatterns\r\n  other-extensions:\r\n                       AllowAmbiguousTypes\r\n                       BangPatterns\r\n                       CPP\r\n                       DeriveLift\r\n                       OverlappingInstances\r\n                       PackageImports\r\n                       RoleAnnotations\r\n                       StaticPointers\r\n                       UndecidableInstances\r\n\r\n  ghc-options:         -Wall\r\n\r\ntest-suite TestSuite\r\n  type:                exitcode-stdio-1.0\r\n  main-is:             TestSuite.hs\r\n  other-modules:       TestSuite.HttpMem\r\n                       TestSuite.InMemCache\r\n                       TestSuite.InMemRepo\r\n                       TestSuite.InMemRepository\r\n                       TestSuite.JSON\r\n                       TestSuite.PrivateKeys\r\n                       TestSuite.Util.StrictMVar\r\n\r\n  -- inherited constraints from lib:hackage-security component\r\n  build-depends:       hackage-security,\r\n                       base,\r\n                       containers,\r\n                       bytestring,\r\n                       network-uri,\r\n                       tar,\r\n                       text,\r\n                       time,\r\n                       zlib\r\n\r\n  if flag(Cabal-syntax)\r\n    build-depends: Cabal        >= 3.7 && < 3.16,\r\n                   Cabal-syntax >= 3.7 && < 3.16\r\n  else\r\n    build-depends: Cabal        >= 2.2.0.1 && < 2.6\r\n                             || >= 3.0     && < 3.7,\r\n                   Cabal-syntax <  3.7\r\n\r\n  -- dependencies exclusive to test-suite\r\n  build-depends:       tasty                 >= 1.1.0.4  && < 1.6,\r\n                         -- tasty-1.1.0.4 is the version in Stackage LTS 12.26 (GHC 8.4)\r\n                       tasty-hunit           == 0.10.*,\r\n                       tasty-quickcheck      >= 0.10     && < 1,\r\n                       QuickCheck            >= 2.11     && < 2.16,\r\n                       aeson                 >= 1.4      && < 1.6 || >= 2.0 && < 2.3,\r\n                       vector                >= 0.12     && < 0.14,\r\n                       unordered-containers  >= 0.2.8.0  && < 0.3,\r\n                       temporary             >= 1.2      && < 1.4\r\n\r\n  hs-source-dirs:      tests\r\n  default-language:    Haskell2010\r\n  default-extensions:  FlexibleContexts\r\n                       GADTs\r\n                       KindSignatures\r\n                       RankNTypes\r\n                       RecordWildCards\r\n                       ScopedTypeVariables\r\n  ghc-options:         -Wall\r\n";
  }