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
    flags = { bounded_memory = false; };
    package = {
      specVersion = "2.2";
      identifier = { name = "hnix-store-core"; version = "0.5.0.0"; };
      license = "Apache-2.0";
      copyright = "2018 Shea Levy";
      maintainer = "shea@shealevy.com";
      author = "Shea Levy";
      homepage = "https://github.com/haskell-nix/hnix-store";
      url = "";
      synopsis = "Core effects for interacting with the Nix store.";
      description = "This package contains types and functions needed to describe\ninteractions with the Nix store abstracted away from\nspecific effectful context.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."attoparsec" or (errorHandler.buildDepError "attoparsec"))
          (hsPkgs."algebraic-graphs" or (errorHandler.buildDepError "algebraic-graphs"))
          (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
          (hsPkgs."base64-bytestring" or (errorHandler.buildDepError "base64-bytestring"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cereal" or (errorHandler.buildDepError "cereal"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."memory" or (errorHandler.buildDepError "memory"))
          (hsPkgs."cryptonite" or (errorHandler.buildDepError "cryptonite"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."lifted-base" or (errorHandler.buildDepError "lifted-base"))
          (hsPkgs."monad-control" or (errorHandler.buildDepError "monad-control"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."nix-derivation" or (errorHandler.buildDepError "nix-derivation"))
          (hsPkgs."saltine" or (errorHandler.buildDepError "saltine"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."unix" or (errorHandler.buildDepError "unix"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          ];
        buildable = true;
        };
      tests = {
        "format-tests" = {
          depends = [
            (hsPkgs."hnix-store-core" or (errorHandler.buildDepError "hnix-store-core"))
            (hsPkgs."attoparsec" or (errorHandler.buildDepError "attoparsec"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
            (hsPkgs."base64-bytestring" or (errorHandler.buildDepError "base64-bytestring"))
            (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."cryptonite" or (errorHandler.buildDepError "cryptonite"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."process" or (errorHandler.buildDepError "process"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-golden" or (errorHandler.buildDepError "tasty-golden"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."tasty-hspec" or (errorHandler.buildDepError "tasty-hspec"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."temporary" or (errorHandler.buildDepError "temporary"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."unix" or (errorHandler.buildDepError "unix"))
            ];
          build-tools = [
            (hsPkgs.buildPackages.tasty-discover.components.exes.tasty-discover or (pkgs.buildPackages.tasty-discover or (errorHandler.buildToolDepError "tasty-discover:tasty-discover")))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/hnix-store-core-0.5.0.0.tar.gz";
      sha256 = "657f7809b1aaae1f24250870014c71f20a9d7a79e3c1dc3c165b2b88cfacb8f0";
      });
    }) // {
    package-description-override = "cabal-version:       2.2\r\nname:                hnix-store-core\r\nversion:             0.5.0.0\r\nx-revision: 2\r\nsynopsis:            Core effects for interacting with the Nix store.\r\ndescription:\r\n  This package contains types and functions needed to describe\r\n  interactions with the Nix store abstracted away from\r\n  specific effectful context.\r\nhomepage:            https://github.com/haskell-nix/hnix-store\r\nlicense:             Apache-2.0\r\nlicense-file:        LICENSE\r\nauthor:              Shea Levy\r\nmaintainer:          shea@shealevy.com\r\ncopyright:           2018 Shea Levy\r\ncategory:            Nix\r\nbuild-type:          Simple\r\nextra-source-files:\r\n    ChangeLog.md\r\n  , README.md\r\n  , tests/samples/example0.drv\r\n  , tests/samples/example1.drv\r\n\r\nCommon commons\r\n  if impl(ghc >= 8.10)\r\n    ghc-options:  -Wall -Wunused-packages\r\n  else\r\n    ghc-options:  -Wall\r\n\r\nlibrary\r\n  import: commons\r\n  exposed-modules:\r\n      System.Nix.Base32\r\n    , System.Nix.Build\r\n    , System.Nix.Derivation\r\n    , System.Nix.Hash\r\n    , System.Nix.Internal.Base\r\n    , System.Nix.Internal.Base32\r\n    , System.Nix.Internal.Truncation\r\n    , System.Nix.Internal.Hash\r\n    , System.Nix.Internal.Nar.Parser\r\n    , System.Nix.Internal.Nar.Streamer\r\n    , System.Nix.Internal.Nar.Effects\r\n    , System.Nix.Internal.Signature\r\n    , System.Nix.Internal.StorePath\r\n    , System.Nix.Nar\r\n    , System.Nix.ReadonlyStore\r\n    , System.Nix.Signature\r\n    , System.Nix.StorePath\r\n    , System.Nix.StorePathMetadata\r\n  build-depends:\r\n      base >=4.11 && <4.16\r\n    , attoparsec < 0.15\r\n    , algebraic-graphs >= 0.5 && < 0.6\r\n    , base16-bytestring < 1.1\r\n    , base64-bytestring < 1.3\r\n    , bytestring < 0.11\r\n    , cereal < 0.6\r\n    , containers < 0.7\r\n    -- Required for cryptonite low-level type convertion\r\n    , memory < 0.17\r\n    , cryptonite < 0.30\r\n    , directory < 1.4\r\n    , filepath < 1.5\r\n    , hashable < 1.5\r\n    , lifted-base < 0.3\r\n    , monad-control < 1.1\r\n    , mtl < 2.3\r\n    , nix-derivation >= 1.1.1 && <2\r\n    , saltine < 0.3\r\n    , time < 1.10\r\n    , text < 1.3\r\n    , unix < 2.8\r\n    , unordered-containers < 0.3\r\n    , vector < 0.13\r\n  hs-source-dirs:      src\r\n  default-language:    Haskell2010\r\n\r\nFlag bounded_memory\r\n  description: Run tests of constant memory use (requires +RTS -T)\r\n  default: False\r\n\r\ntest-suite format-tests\r\n  import: commons\r\n  if flag(bounded_memory)\r\n    cpp-options: -DBOUNDED_MEMORY\r\n    ghc-options: -rtsopts -fprof-auto\r\n  type: exitcode-stdio-1.0\r\n  main-is: Driver.hs\r\n  other-modules:\r\n    Arbitrary\r\n    Derivation\r\n    NarFormat\r\n    Hash\r\n    StorePath\r\n  hs-source-dirs:\r\n    tests\r\n  build-tool-depends:\r\n    tasty-discover:tasty-discover\r\n  build-depends:\r\n      hnix-store-core\r\n    , attoparsec\r\n    , base\r\n    , base16-bytestring\r\n    , base64-bytestring\r\n    , binary\r\n    , bytestring\r\n    , containers\r\n    , cryptonite\r\n    , directory\r\n    , filepath\r\n    , process\r\n    , tasty\r\n    , tasty-golden\r\n    , hspec\r\n    , tasty-hspec\r\n    , tasty-hunit\r\n    , tasty-quickcheck\r\n    , temporary\r\n    , text\r\n    , unix\r\n  default-language: Haskell2010\r\n";
    }