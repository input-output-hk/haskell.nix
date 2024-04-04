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
      identifier = { name = "hackage-security"; version = "0.6.2.3"; };
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
      url = "http://hackage.haskell.org/package/hackage-security-0.6.2.3.tar.gz";
      sha256 = "52ee0576971955571d846b8e6c09638f89f4f7881f4a95173e44ccc0d856a066";
      });
    }) // {
    package-description-override = "cabal-version:       1.12\nname:                hackage-security\nversion:             0.6.2.3\nx-revision:          8\n\nsynopsis:            Hackage security library\ndescription:         The hackage security library provides both server and\n                     client utilities for securing the Hackage package server\n                     (<https://hackage.haskell.org/>).  It is based on The Update\n                     Framework (<https://theupdateframework.com/>), a set of\n                     recommendations developed by security researchers at\n                     various universities in the US as well as developers on the\n                     Tor project (<https://www.torproject.org/>).\n                     .\n                     The current implementation supports only index signing,\n                     thereby enabling untrusted mirrors. It does not yet provide\n                     facilities for author package signing.\n                     .\n                     The library has two main entry points:\n                     \"Hackage.Security.Client\" is the main entry point for\n                     clients (the typical example being @cabal@), and\n                     \"Hackage.Security.Server\" is the main entry point for\n                     servers (the typical example being @hackage-server@).\nlicense:             BSD3\nlicense-file:        LICENSE\nauthor:              Edsko de Vries\nmaintainer:          cabal-devel@haskell.org\ncopyright:           Copyright 2015-2022 Well-Typed LLP\ncategory:            Distribution\nhomepage:            https://github.com/haskell/hackage-security\nbug-reports:         https://github.com/haskell/hackage-security/issues\nbuild-type:          Simple\n\ntested-with:\n  GHC == 9.8.1\n  GHC == 9.6.3\n  GHC == 9.4.7\n  GHC == 9.2.8\n  GHC == 9.0.2\n  GHC == 8.10.7\n  GHC == 8.8.4\n  GHC == 8.6.5\n  GHC == 8.4.4\n  GHC == 8.2.2\n  GHC == 8.0.2\n  GHC == 7.10.3\n\nextra-source-files:\n  ChangeLog.md\n\nsource-repository head\n  type: git\n  location: https://github.com/haskell/hackage-security.git\n\nflag base48\n  description: Are we using @base@ 4.8 or later?\n  manual: False\n\nflag use-network-uri\n  description: Are we using @network-uri@?\n  manual: False\n\nflag Cabal-syntax\n  description: Are we using Cabal-syntax?\n  manual: False\n  default: False\n\nflag old-directory\n  description: Use @directory@ < 1.2 and @old-time@\n  manual:      False\n  default:     False\n\nflag mtl21\n  description: Use @mtl@ < 2.2 and @mtl-compat@\n  manual:      False\n  default:     False\n\nflag lukko\n  description: Use @lukko@ for file-locking, otherwise use @GHC.IO.Handle.Lock@\n  manual:      True\n  default:     True\n\nlibrary\n  -- Most functionality is exported through the top-level entry points .Client\n  -- and .Server; the other exported modules are intended for qualified imports.\n  exposed-modules:     Hackage.Security.Client\n                       Hackage.Security.Client.Formats\n                       Hackage.Security.Client.Repository\n                       Hackage.Security.Client.Repository.Cache\n                       Hackage.Security.Client.Repository.Local\n                       Hackage.Security.Client.Repository.Remote\n                       Hackage.Security.Client.Repository.HttpLib\n                       Hackage.Security.Client.Verify\n                       Hackage.Security.JSON\n                       Hackage.Security.Key.Env\n                       Hackage.Security.Server\n                       Hackage.Security.Trusted\n                       Hackage.Security.TUF.FileMap\n                       Hackage.Security.Util.Checked\n                       Hackage.Security.Util.Path\n                       Hackage.Security.Util.Pretty\n                       Hackage.Security.Util.Some\n                       Text.JSON.Canonical\n  other-modules:       Hackage.Security.Key\n                       Hackage.Security.Trusted.TCB\n                       Hackage.Security.TUF\n                       Hackage.Security.TUF.Common\n                       Hackage.Security.TUF.FileInfo\n                       Hackage.Security.TUF.Header\n                       Hackage.Security.TUF.Layout.Cache\n                       Hackage.Security.TUF.Layout.Index\n                       Hackage.Security.TUF.Layout.Repo\n                       Hackage.Security.TUF.Mirrors\n                       Hackage.Security.TUF.Paths\n                       Hackage.Security.TUF.Patterns\n                       Hackage.Security.TUF.Root\n                       Hackage.Security.TUF.Signed\n                       Hackage.Security.TUF.Snapshot\n                       Hackage.Security.TUF.Targets\n                       Hackage.Security.TUF.Timestamp\n                       Hackage.Security.Util.Base64\n                       Hackage.Security.Util.Exit\n                       Hackage.Security.Util.IO\n                       Hackage.Security.Util.JSON\n                       Hackage.Security.Util.Lens\n                       Hackage.Security.Util.Stack\n                       Hackage.Security.Util.TypedEmbedded\n                       MyPrelude\n  -- We support ghc 7.4 (bundled with Cabal 1.14) and up\n  build-depends:       base              >= 4.5     && < 4.20,\n                       base16-bytestring >= 0.1.1   && < 1.1,\n                       base64-bytestring >= 1.0     && < 1.3,\n                       bytestring        >= 0.9     && < 0.13,\n                       containers        >= 0.4     && < 0.8,\n                       ed25519           >= 0.0     && < 0.1,\n                       filepath          >= 1.2     && < 1.5,\n                       parsec            >= 3.1     && < 3.2,\n                       pretty            >= 1.0     && < 1.2,\n                       cryptohash-sha256 >= 0.11    && < 0.12,\n                       -- 0.4.2 introduces TarIndex, 0.4.4 introduces more\n                       -- functionality, 0.5.0 changes type of serialise\n                       tar               >= 0.5     && < 0.6,\n                       template-haskell  >= 2.7     && < 2.22,\n                       time              >= 1.2     && < 1.13,\n                       transformers      >= 0.3     && < 0.7,\n                       zlib              >= 0.5     && < 0.7,\n                       -- whatever versions are bundled with ghc:\n                       ghc-prim\n  if flag(old-directory)\n    build-depends:     directory  >= 1.1.0.2 && < 1.2,\n                       old-time   >= 1 &&       < 1.2\n  else\n    build-depends:     directory  >= 1.2 && < 1.4\n\n  if flag(mtl21)\n    build-depends:     mtl        >= 2.1     && < 2.2,\n                       mtl-compat >= 0.2     && < 0.3\n  else\n    build-depends:     mtl        >= 2.2     && < 2.4\n\n  if flag(lukko)\n    build-depends:     lukko      >= 0.1     && < 0.2\n  else\n    build-depends:     base       >= 4.10\n\n  if flag(Cabal-syntax) && impl(ghc >= 8.2)\n    build-depends: Cabal-syntax >= 3.7 && < 3.12\n  else\n    build-depends: Cabal        >= 1.14    && < 1.26\n                             || >= 2.0     && < 2.6\n                             || >= 3.0     && < 3.7,\n                   Cabal-syntax <  3.7\n\n  hs-source-dirs:      src\n  default-language:    Haskell2010\n  default-extensions:  DefaultSignatures\n                       DeriveDataTypeable\n                       DeriveFunctor\n                       FlexibleContexts\n                       FlexibleInstances\n                       GADTs\n                       GeneralizedNewtypeDeriving\n                       KindSignatures\n                       MultiParamTypeClasses\n                       NamedFieldPuns\n                       NoImplicitPrelude\n                       NoMonomorphismRestriction\n                       RankNTypes\n                       RecordWildCards\n                       ScopedTypeVariables\n                       StandaloneDeriving\n                       TupleSections\n                       TypeFamilies\n                       TypeOperators\n                       ViewPatterns\n  other-extensions:    BangPatterns\n                       CPP\n                       OverlappingInstances\n                       PackageImports\n                       UndecidableInstances\n\n  -- use the new stage1/cross-compile-friendly DeriveLift extension for GHC 8.0+\n  if impl(ghc >= 8.0)\n    other-extensions: DeriveLift\n  else\n    other-extensions: TemplateHaskell\n\n  ghc-options:         -Wall\n\n  if flag(base48)\n    build-depends: base >= 4.8\n  else\n    build-depends: base < 4.8, old-locale == 1.0.*\n\n  -- The URI type got split out off the network package after version 2.5, and\n  -- moved to a separate network-uri package. Since we don't need the rest of\n  -- network here, it would suffice to rely only on network-uri:\n  --\n  -- > if flag(use-network-uri)\n  -- >   build-depends: network-uri >= 2.6 && < 2.7\n  -- > else\n  -- >   build-depends: network     >= 2.5 && < 2.6\n  --\n  -- However, if we did the same in hackage-security-HTTP, Cabal would consider\n  -- those two flag choices (hackage-security:use-network-uri and\n  -- hackage-security-HTTP:use-network-uri) to be completely independent; but\n  -- they aren't: if it links hackage-security against network-uri and\n  -- hackage-security-HTTP against network, we will get type errors when\n  -- hackage-security-HTTP tries to pass a URI to hackage-security.\n  --\n  -- It might seem we can solve this problem by re-exporting the URI type in\n  -- hackage-security and avoid the dependency in hackage-security-HTTP\n  -- altogether. However, this merely shifts the problem: hackage-security-HTTP\n  -- relies on the HTTP library which--surprise!--makes the same choice between\n  -- depending on network or network-uri. Cabal will not notice that we cannot\n  -- build hackage-security and hackage-security-HTTP against network-uri but\n  -- HTTP against network.\n  --\n  -- We solve the problem by explicitly relying on network-2.6 when choosing\n  -- network-uri. This dependency is redundant, strictly speaking. However, it\n  -- serves as a proxy for forcing flag choices: since all packages in a\n  -- solution must be linked against the same version of network, having one\n  -- version of network in one branch of the conditional and another version of\n  -- network in the other branch forces the choice to be consistent throughout.\n  -- (Note that the HTTP library does the same thing, though in this case the\n  -- dependency in network is not redundant.)\n  if flag(use-network-uri)\n    build-depends: network-uri >= 2.6 && < 2.7,\n                   network     >= 2.6 && < 2.9\n                            || >= 3.0 && < 3.2\n  else\n    build-depends: network     >= 2.5 && < 2.6\n\n  if impl(ghc >= 7.8)\n     other-extensions: RoleAnnotations\n\n  if impl(ghc >= 7.10)\n     other-extensions: AllowAmbiguousTypes\n                       StaticPointers\n\ntest-suite TestSuite\n  type:                exitcode-stdio-1.0\n  main-is:             TestSuite.hs\n  other-modules:       TestSuite.HttpMem\n                       TestSuite.InMemCache\n                       TestSuite.InMemRepo\n                       TestSuite.InMemRepository\n                       TestSuite.JSON\n                       TestSuite.PrivateKeys\n                       TestSuite.Util.StrictMVar\n\n  -- inherited constraints from lib:hackage-security component\n  build-depends:       hackage-security,\n                       base,\n                       containers,\n                       bytestring,\n                       network-uri,\n                       tar,\n                       text,\n                       time,\n                       zlib\n\n  if flag(Cabal-syntax) && impl(ghc >= 8.2)\n    build-depends: Cabal        >= 3.7 && < 3.12,\n                   Cabal-syntax >= 3.7 && < 3.12\n  else\n    build-depends: Cabal        >= 1.14    && < 1.26\n                             || >= 2.0     && < 2.6\n                             || >= 3.0     && < 3.7,\n                   Cabal-syntax <  3.7\n\n  -- dependencies exclusive to test-suite\n  build-depends:       tasty            >= 1.2 && < 1.6,\n                       tasty-hunit      == 0.10.*,\n                       tasty-quickcheck == 0.10.*,\n                       QuickCheck       >= 2.11 && <2.15,\n                       aeson            >= 1.4 && < 1.6 || >= 2.0 && < 2.3,\n                       vector           >= 0.12 && <0.14,\n                       unordered-containers >=0.2.8.0 && <0.3,\n                       temporary        >= 1.2 && < 1.4\n\n  hs-source-dirs:      tests\n  default-language:    Haskell2010\n  default-extensions:  FlexibleContexts\n                       GADTs\n                       KindSignatures\n                       RankNTypes\n                       RecordWildCards\n                       ScopedTypeVariables\n  ghc-options:         -Wall\n";
    }