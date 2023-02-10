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
      identifier = { name = "hpack"; version = "0.35.0"; };
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
          (hsPkgs."cryptonite" or (errorHandler.buildDepError "cryptonite"))
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
          ];
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
            (hsPkgs."cryptonite" or (errorHandler.buildDepError "cryptonite"))
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
            ];
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
            (hsPkgs."cryptonite" or (errorHandler.buildDepError "cryptonite"))
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
            ];
          build-tools = [
            (hsPkgs.buildPackages.hspec-discover.components.exes.hspec-discover or (pkgs.buildPackages.hspec-discover or (errorHandler.buildToolDepError "hspec-discover:hspec-discover")))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/hpack-0.35.0.tar.gz";
      sha256 = "c6bdbc2d48dac398d3c35120a11455f507639f275befc4fda0a26662db2231b2";
      });
    }) // {
    package-description-override = "cabal-version: 1.12\r\n\r\n-- This file has been generated from package.yaml by hpack version 0.34.7.\r\n--\r\n-- see: https://github.com/sol/hpack\r\n\r\nname:           hpack\r\nversion:        0.35.0\r\nx-revision: 1\r\nsynopsis:       A modern format for Haskell packages\r\ndescription:    See README at <https://github.com/sol/hpack#readme>\r\ncategory:       Development\r\nhomepage:       https://github.com/sol/hpack#readme\r\nbug-reports:    https://github.com/sol/hpack/issues\r\nmaintainer:     Simon Hengel <sol@typeful.net>\r\nlicense:        MIT\r\nlicense-file:   LICENSE\r\nbuild-type:     Simple\r\nextra-source-files:\r\n    CHANGELOG.md\r\n\r\nsource-repository head\r\n  type: git\r\n  location: https://github.com/sol/hpack\r\n\r\nlibrary\r\n  hs-source-dirs:\r\n      src\r\n  ghc-options: -Wall -fno-warn-incomplete-uni-patterns\r\n  build-depends:\r\n      Cabal >=3.0.0.0 && <3.9\r\n    , Glob >=0.9.0\r\n    , aeson >=1.4.3.0\r\n    , base >=4.9 && <5\r\n    , bifunctors\r\n    , bytestring\r\n    , containers\r\n    , cryptonite\r\n    , deepseq\r\n    , directory >=1.2.5.0\r\n    , filepath\r\n    , http-client\r\n    , http-client-tls\r\n    , http-types\r\n    , infer-license >=0.2.0 && <0.3\r\n    , pretty\r\n    , scientific\r\n    , text\r\n    , transformers\r\n    , unordered-containers\r\n    , vector\r\n    , yaml >=0.10.0\r\n  exposed-modules:\r\n      Hpack\r\n      Hpack.Config\r\n      Hpack.Render\r\n      Hpack.Yaml\r\n  other-modules:\r\n      Data.Aeson.Config.FromValue\r\n      Data.Aeson.Config.Key\r\n      Data.Aeson.Config.KeyMap\r\n      Data.Aeson.Config.Parser\r\n      Data.Aeson.Config.Types\r\n      Data.Aeson.Config.Util\r\n      Hpack.CabalFile\r\n      Hpack.Defaults\r\n      Hpack.Haskell\r\n      Hpack.License\r\n      Hpack.Module\r\n      Hpack.Options\r\n      Hpack.Render.Dsl\r\n      Hpack.Render.Hints\r\n      Hpack.Syntax.BuildTools\r\n      Hpack.Syntax.Defaults\r\n      Hpack.Syntax.Dependencies\r\n      Hpack.Syntax.DependencyVersion\r\n      Hpack.Syntax.Git\r\n      Hpack.Syntax.ParseDependencies\r\n      Hpack.Utf8\r\n      Hpack.Util\r\n      Imports\r\n      Path\r\n      Paths_hpack\r\n  default-language: Haskell2010\r\n\r\nexecutable hpack\r\n  main-is: Main.hs\r\n  hs-source-dirs:\r\n      driver\r\n  ghc-options: -Wall -fno-warn-incomplete-uni-patterns\r\n  build-depends:\r\n      Cabal >=3.0.0.0 && <3.9\r\n    , Glob >=0.9.0\r\n    , aeson >=1.4.3.0\r\n    , base >=4.9 && <5\r\n    , bifunctors\r\n    , bytestring\r\n    , containers\r\n    , cryptonite\r\n    , deepseq\r\n    , directory >=1.2.5.0\r\n    , filepath\r\n    , hpack\r\n    , http-client\r\n    , http-client-tls\r\n    , http-types\r\n    , infer-license >=0.2.0 && <0.3\r\n    , pretty\r\n    , scientific\r\n    , text\r\n    , transformers\r\n    , unordered-containers\r\n    , vector\r\n    , yaml >=0.10.0\r\n  other-modules:\r\n      Paths_hpack\r\n  default-language: Haskell2010\r\n\r\ntest-suite spec\r\n  type: exitcode-stdio-1.0\r\n  main-is: Spec.hs\r\n  hs-source-dirs:\r\n      test\r\n      src\r\n  ghc-options: -Wall -fno-warn-incomplete-uni-patterns\r\n  cpp-options: -DTEST\r\n  build-depends:\r\n      Cabal >=3.0.0.0 && <3.9\r\n    , Glob >=0.9.0\r\n    , HUnit >=1.6.0.0\r\n    , QuickCheck\r\n    , aeson >=1.4.3.0\r\n    , base >=4.9 && <5\r\n    , bifunctors\r\n    , bytestring\r\n    , containers\r\n    , cryptonite\r\n    , deepseq\r\n    , directory >=1.2.5.0\r\n    , filepath\r\n    , hspec ==2.*\r\n    , http-client\r\n    , http-client-tls\r\n    , http-types\r\n    , infer-license >=0.2.0 && <0.3\r\n    , interpolate\r\n    , mockery >=0.3\r\n    , pretty\r\n    , scientific\r\n    , template-haskell\r\n    , temporary\r\n    , text\r\n    , transformers\r\n    , unordered-containers\r\n    , vector\r\n    , yaml >=0.10.0\r\n  build-tool-depends:\r\n      hspec-discover:hspec-discover\r\n  other-modules:\r\n      Data.Aeson.Config.FromValueSpec\r\n      Data.Aeson.Config.TypesSpec\r\n      Data.Aeson.Config.UtilSpec\r\n      EndToEndSpec\r\n      Helper\r\n      Hpack.CabalFileSpec\r\n      Hpack.ConfigSpec\r\n      Hpack.DefaultsSpec\r\n      Hpack.HaskellSpec\r\n      Hpack.LicenseSpec\r\n      Hpack.ModuleSpec\r\n      Hpack.OptionsSpec\r\n      Hpack.Render.DslSpec\r\n      Hpack.Render.HintsSpec\r\n      Hpack.RenderSpec\r\n      Hpack.Syntax.BuildToolsSpec\r\n      Hpack.Syntax.DefaultsSpec\r\n      Hpack.Syntax.DependenciesSpec\r\n      Hpack.Syntax.GitSpec\r\n      Hpack.Utf8Spec\r\n      Hpack.UtilSpec\r\n      HpackSpec\r\n      Data.Aeson.Config.FromValue\r\n      Data.Aeson.Config.Key\r\n      Data.Aeson.Config.KeyMap\r\n      Data.Aeson.Config.Parser\r\n      Data.Aeson.Config.Types\r\n      Data.Aeson.Config.Util\r\n      Hpack\r\n      Hpack.CabalFile\r\n      Hpack.Config\r\n      Hpack.Defaults\r\n      Hpack.Haskell\r\n      Hpack.License\r\n      Hpack.Module\r\n      Hpack.Options\r\n      Hpack.Render\r\n      Hpack.Render.Dsl\r\n      Hpack.Render.Hints\r\n      Hpack.Syntax.BuildTools\r\n      Hpack.Syntax.Defaults\r\n      Hpack.Syntax.Dependencies\r\n      Hpack.Syntax.DependencyVersion\r\n      Hpack.Syntax.Git\r\n      Hpack.Syntax.ParseDependencies\r\n      Hpack.Utf8\r\n      Hpack.Util\r\n      Hpack.Yaml\r\n      Imports\r\n      Path\r\n      Paths_hpack\r\n  default-language: Haskell2010\r\n";
    }