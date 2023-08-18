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
    }) // {
    package-description-override = "cabal-version:       2.2\nname:                hnix-store-remote\nversion:             0.5.0.0\nsynopsis:            Remote hnix store\ndescription:         Implementation of the nix store using the daemon protocol.\nhomepage:            https://github.com/haskell-nix/hnix-store\nlicense:             Apache-2.0\nlicense-file:        LICENSE\nauthor:              Richard Marko\nmaintainer:          srk@48.io\ncopyright:           2018 Richard Marko\ncategory:            Nix\nbuild-type:          Simple\nextra-source-files:  ChangeLog.md, README.md\n\nCommon commons\n  if impl(ghc >= 8.10)\n    ghc-options:  -Wall -Wunused-packages\n  else\n    ghc-options:  -Wall\n\nflag io-testsuite\n  default:\n    False\n  description:\n    Enable testsuite, which requires external\n    binaries and Linux namespace support.\n\nlibrary\n  import: commons\n  exposed-modules:\n      System.Nix.Store.Remote\n    , System.Nix.Store.Remote.Binary\n    , System.Nix.Store.Remote.Builders\n    , System.Nix.Store.Remote.Logger\n    , System.Nix.Store.Remote.Parsers\n    , System.Nix.Store.Remote.Protocol\n    , System.Nix.Store.Remote.Types\n    , System.Nix.Store.Remote.Util\n\n  build-depends:\n      base >=4.10 && <5\n    , attoparsec\n    , binary\n    , bytestring\n    , containers\n    , cryptonite\n    , text\n    , time\n    , network\n    , nix-derivation >= 1.1.1 && <2\n    , mtl\n    , unordered-containers\n    , hnix-store-core >= 0.5 && <0.6\n  hs-source-dirs:      src\n  default-language:    Haskell2010\n  ghc-options:         -Wall\n\ntest-suite hnix-store-remote-tests\n  import: commons\n\n  if !flag(io-testsuite)\n    buildable: False\n    ghc-options:       -rtsopts -fprof-auto\n\n  type:              exitcode-stdio-1.0\n  main-is:           Driver.hs\n  other-modules:\n      Derivation\n    , NixDaemon\n    , Spec\n    , Util\n  hs-source-dirs:    tests\n  build-tool-depends:\n    tasty-discover:tasty-discover\n  build-depends:\n      base\n    , hnix-store-core >= 0.3\n    , hnix-store-remote\n    , containers\n    , cryptonite\n    , directory\n    , process\n    , filepath\n    , hspec-expectations-lifted\n    , quickcheck-text\n    , tasty\n    , hspec\n    , tasty-hspec\n    , tasty-quickcheck\n    , linux-namespaces\n    , nix-derivation\n    , temporary\n    , text\n    , unix\n    , unordered-containers\n    , vector\n  default-language: Haskell2010\n";
    }