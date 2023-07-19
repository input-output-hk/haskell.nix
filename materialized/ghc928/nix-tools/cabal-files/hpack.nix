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
    flags = {};
    package = {
      specVersion = "1.12";
      identifier = { name = "hpack"; version = "0.35.3"; };
      license = "MIT";
      copyright = "";
      maintainer = "Simon Hengel <sol@typeful.net>";
      author = "";
      homepage = "https://github.com/sol/hpack#readme";
      url = "";
      synopsis = "A modern format for Haskell packages";
      description = "See README at <https://github.com/sol/hpack#readme>";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."Cabal" or (errorHandler.buildDepError "Cabal"))
          (hsPkgs."Glob" or (errorHandler.buildDepError "Glob"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bifunctors" or (errorHandler.buildDepError "bifunctors"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."crypton" or (errorHandler.buildDepError "crypton"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."http-client" or (errorHandler.buildDepError "http-client"))
          (hsPkgs."http-client-tls" or (errorHandler.buildDepError "http-client-tls"))
          (hsPkgs."http-types" or (errorHandler.buildDepError "http-types"))
          (hsPkgs."infer-license" or (errorHandler.buildDepError "infer-license"))
          (hsPkgs."pretty" or (errorHandler.buildDepError "pretty"))
          (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          (hsPkgs."yaml" or (errorHandler.buildDepError "yaml"))
          ] ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).ge "9.4.5" && system.isWindows) (hsPkgs."network" or (errorHandler.buildDepError "network"));
        buildable = true;
        };
      exes = {
        "hpack" = {
          depends = [
            (hsPkgs."Cabal" or (errorHandler.buildDepError "Cabal"))
            (hsPkgs."Glob" or (errorHandler.buildDepError "Glob"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bifunctors" or (errorHandler.buildDepError "bifunctors"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."crypton" or (errorHandler.buildDepError "crypton"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."hpack" or (errorHandler.buildDepError "hpack"))
            (hsPkgs."http-client" or (errorHandler.buildDepError "http-client"))
            (hsPkgs."http-client-tls" or (errorHandler.buildDepError "http-client-tls"))
            (hsPkgs."http-types" or (errorHandler.buildDepError "http-types"))
            (hsPkgs."infer-license" or (errorHandler.buildDepError "infer-license"))
            (hsPkgs."pretty" or (errorHandler.buildDepError "pretty"))
            (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."yaml" or (errorHandler.buildDepError "yaml"))
            ] ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).ge "9.4.5" && system.isWindows) (hsPkgs."network" or (errorHandler.buildDepError "network"));
          buildable = true;
          };
        };
      tests = {
        "spec" = {
          depends = [
            (hsPkgs."Cabal" or (errorHandler.buildDepError "Cabal"))
            (hsPkgs."Glob" or (errorHandler.buildDepError "Glob"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bifunctors" or (errorHandler.buildDepError "bifunctors"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."crypton" or (errorHandler.buildDepError "crypton"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."http-client" or (errorHandler.buildDepError "http-client"))
            (hsPkgs."http-client-tls" or (errorHandler.buildDepError "http-client-tls"))
            (hsPkgs."http-types" or (errorHandler.buildDepError "http-types"))
            (hsPkgs."infer-license" or (errorHandler.buildDepError "infer-license"))
            (hsPkgs."interpolate" or (errorHandler.buildDepError "interpolate"))
            (hsPkgs."mockery" or (errorHandler.buildDepError "mockery"))
            (hsPkgs."pretty" or (errorHandler.buildDepError "pretty"))
            (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
            (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
            (hsPkgs."temporary" or (errorHandler.buildDepError "temporary"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."yaml" or (errorHandler.buildDepError "yaml"))
            ] ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).ge "9.4.5" && system.isWindows) (hsPkgs."network" or (errorHandler.buildDepError "network"));
          build-tools = [
            (hsPkgs.buildPackages.hspec-discover.components.exes.hspec-discover or (pkgs.buildPackages.hspec-discover or (errorHandler.buildToolDepError "hspec-discover:hspec-discover")))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/hpack-0.35.3.tar.gz";
      sha256 = "24cdb20a4ce8486873862a0e122b256a2f060c26b96326e1386e7822a1d805ce";
      });
    }) // {
    package-description-override = "cabal-version: 1.12\n\n-- This file has been generated from package.yaml by hpack version 0.35.2.\n--\n-- see: https://github.com/sol/hpack\n\nname:           hpack\nversion:        0.35.3\nsynopsis:       A modern format for Haskell packages\ndescription:    See README at <https://github.com/sol/hpack#readme>\ncategory:       Development\nhomepage:       https://github.com/sol/hpack#readme\nbug-reports:    https://github.com/sol/hpack/issues\nmaintainer:     Simon Hengel <sol@typeful.net>\nlicense:        MIT\nlicense-file:   LICENSE\nbuild-type:     Simple\nextra-source-files:\n    CHANGELOG.md\n    resources/test/hpack.cabal\n\nsource-repository head\n  type: git\n  location: https://github.com/sol/hpack\n\nlibrary\n  hs-source-dirs:\n      src\n  ghc-options: -Wall -fno-warn-incomplete-uni-patterns\n  build-depends:\n      Cabal >=3.0.0.0 && <3.11\n    , Glob >=0.9.0\n    , aeson >=1.4.3.0\n    , base >=4.13 && <5\n    , bifunctors\n    , bytestring\n    , containers\n    , crypton\n    , deepseq\n    , directory >=1.2.5.0\n    , filepath\n    , http-client\n    , http-client-tls >=0.3.6.2\n    , http-types\n    , infer-license >=0.2.0 && <0.3\n    , pretty\n    , scientific\n    , text\n    , transformers\n    , unordered-containers\n    , vector\n    , yaml >=0.10.0\n  exposed-modules:\n      Hpack\n      Hpack.Config\n      Hpack.Render\n      Hpack.Yaml\n      Hpack.Error\n  other-modules:\n      Data.Aeson.Config.FromValue\n      Data.Aeson.Config.Key\n      Data.Aeson.Config.KeyMap\n      Data.Aeson.Config.Parser\n      Data.Aeson.Config.Types\n      Data.Aeson.Config.Util\n      Hpack.CabalFile\n      Hpack.Defaults\n      Hpack.Haskell\n      Hpack.License\n      Hpack.Module\n      Hpack.Options\n      Hpack.Render.Dsl\n      Hpack.Render.Hints\n      Hpack.Syntax.BuildTools\n      Hpack.Syntax.Defaults\n      Hpack.Syntax.Dependencies\n      Hpack.Syntax.DependencyVersion\n      Hpack.Syntax.Git\n      Hpack.Syntax.ParseDependencies\n      Hpack.Utf8\n      Hpack.Util\n      Imports\n      Path\n      Paths_hpack\n  default-language: Haskell2010\n  if impl(ghc >= 9.4.5) && os(windows)\n    build-depends:\n        network >=3.1.2.9\n\nexecutable hpack\n  main-is: Main.hs\n  hs-source-dirs:\n      driver\n  ghc-options: -Wall -fno-warn-incomplete-uni-patterns\n  build-depends:\n      Cabal >=3.0.0.0 && <3.11\n    , Glob >=0.9.0\n    , aeson >=1.4.3.0\n    , base >=4.13 && <5\n    , bifunctors\n    , bytestring\n    , containers\n    , crypton\n    , deepseq\n    , directory >=1.2.5.0\n    , filepath\n    , hpack\n    , http-client\n    , http-client-tls >=0.3.6.2\n    , http-types\n    , infer-license >=0.2.0 && <0.3\n    , pretty\n    , scientific\n    , text\n    , transformers\n    , unordered-containers\n    , vector\n    , yaml >=0.10.0\n  other-modules:\n      Paths_hpack\n  default-language: Haskell2010\n  if impl(ghc >= 9.4.5) && os(windows)\n    build-depends:\n        network >=3.1.2.9\n\ntest-suite spec\n  type: exitcode-stdio-1.0\n  main-is: Spec.hs\n  hs-source-dirs:\n      test\n      src\n  ghc-options: -Wall -fno-warn-incomplete-uni-patterns\n  cpp-options: -DTEST\n  build-depends:\n      Cabal >=3.0.0.0 && <3.11\n    , Glob >=0.9.0\n    , HUnit >=1.6.0.0\n    , QuickCheck\n    , aeson >=1.4.3.0\n    , base >=4.13 && <5\n    , bifunctors\n    , bytestring\n    , containers\n    , crypton\n    , deepseq\n    , directory >=1.2.5.0\n    , filepath\n    , hspec ==2.*\n    , http-client\n    , http-client-tls >=0.3.6.2\n    , http-types\n    , infer-license >=0.2.0 && <0.3\n    , interpolate\n    , mockery >=0.3\n    , pretty\n    , scientific\n    , template-haskell\n    , temporary\n    , text\n    , transformers\n    , unordered-containers\n    , vector\n    , yaml >=0.10.0\n  build-tool-depends:\n      hspec-discover:hspec-discover\n  other-modules:\n      Data.Aeson.Config.FromValueSpec\n      Data.Aeson.Config.TypesSpec\n      Data.Aeson.Config.UtilSpec\n      EndToEndSpec\n      Helper\n      Hpack.CabalFileSpec\n      Hpack.ConfigSpec\n      Hpack.DefaultsSpec\n      Hpack.HaskellSpec\n      Hpack.LicenseSpec\n      Hpack.ModuleSpec\n      Hpack.OptionsSpec\n      Hpack.Render.DslSpec\n      Hpack.Render.HintsSpec\n      Hpack.RenderSpec\n      Hpack.Syntax.BuildToolsSpec\n      Hpack.Syntax.DefaultsSpec\n      Hpack.Syntax.DependenciesSpec\n      Hpack.Syntax.GitSpec\n      Hpack.Utf8Spec\n      Hpack.UtilSpec\n      HpackSpec\n      Data.Aeson.Config.FromValue\n      Data.Aeson.Config.Key\n      Data.Aeson.Config.KeyMap\n      Data.Aeson.Config.Parser\n      Data.Aeson.Config.Types\n      Data.Aeson.Config.Util\n      Hpack\n      Hpack.CabalFile\n      Hpack.Config\n      Hpack.Defaults\n      Hpack.Error\n      Hpack.Haskell\n      Hpack.License\n      Hpack.Module\n      Hpack.Options\n      Hpack.Render\n      Hpack.Render.Dsl\n      Hpack.Render.Hints\n      Hpack.Syntax.BuildTools\n      Hpack.Syntax.Defaults\n      Hpack.Syntax.Dependencies\n      Hpack.Syntax.DependencyVersion\n      Hpack.Syntax.Git\n      Hpack.Syntax.ParseDependencies\n      Hpack.Utf8\n      Hpack.Util\n      Hpack.Yaml\n      Imports\n      Path\n      Paths_hpack\n  default-language: Haskell2010\n  if impl(ghc >= 9.4.5) && os(windows)\n    build-depends:\n        network >=3.1.2.9\n";
    }