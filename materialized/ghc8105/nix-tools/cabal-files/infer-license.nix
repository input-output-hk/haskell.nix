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
      specVersion = "1.10";
      identifier = { name = "infer-license"; version = "0.2.0"; };
      license = "MIT";
      copyright = "(c) 2018 Simon Hengel";
      maintainer = "Simon Hengel <sol@typeful.net>";
      author = "Simon Hengel <sol@typeful.net>";
      homepage = "";
      url = "";
      synopsis = "Infer software license from a given license file";
      description = "";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."text-metrics" or (errorHandler.buildDepError "text-metrics"))
          ];
        buildable = true;
        };
      tests = {
        "spec" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."infer-license" or (errorHandler.buildDepError "infer-license"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."text-metrics" or (errorHandler.buildDepError "text-metrics"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/infer-license-0.2.0.tar.gz";
      sha256 = "12e6fe616575159c03cf6fd4a4f30021ecf264a529ab4e4edd6e96e296a98e72";
      });
    }) // {
    package-description-override = "cabal-version: >= 1.10\n\n-- This file has been generated from package.yaml by hpack version 0.29.3.\n--\n-- see: https://github.com/sol/hpack\n--\n-- hash: d60db93dd346c9277287cf8528ebcb3c62202cb8c214322bf6557d80c687f737\n\nname:           infer-license\nversion:        0.2.0\nsynopsis:       Infer software license from a given license file\ncategory:       Development\nbug-reports:    https://github.com/sol/infer-license/issues\nauthor:         Simon Hengel <sol@typeful.net>\nmaintainer:     Simon Hengel <sol@typeful.net>\ncopyright:      (c) 2018 Simon Hengel\nlicense:        MIT\nlicense-file:   LICENSE\nbuild-type:     Simple\nextra-source-files:\n    test/resources/AGPLv3\n    test/resources/Apache_2_0\n    test/resources/BSD2\n    test/resources/BSD3\n    test/resources/BSD4\n    test/resources/GPLv2\n    test/resources/GPLv3\n    test/resources/ISC/me\n    test/resources/ISC/spdx\n    test/resources/LGPLv2_1\n    test/resources/LGPLv3\n    test/resources/MIT\n    test/resources/MPL_2_0\n\nsource-repository head\n  type: git\n  location: https://github.com/sol/infer-license\n\nlibrary\n  exposed-modules:\n      Data.License.Infer\n  other-modules:\n      Data.License.SpdxLicenses\n      Data.License.Type\n      Paths_infer_license\n  hs-source-dirs:\n      src\n  ghc-options: -Wall\n  build-depends:\n      base >=4.9 && <5\n    , directory >=1.2.5.0\n    , filepath\n    , text\n    , text-metrics >=0.3.0 && <0.4\n  default-language: Haskell2010\n\ntest-suite spec\n  type: exitcode-stdio-1.0\n  main-is: Spec.hs\n  other-modules:\n      Data.License.InferSpec\n      Paths_infer_license\n  hs-source-dirs:\n      test\n  ghc-options: -Wall\n  build-depends:\n      base >=4.9 && <5\n    , directory >=1.2.5.0\n    , filepath\n    , hspec ==2.*\n    , infer-license\n    , text\n    , text-metrics >=0.3.0 && <0.4\n  default-language: Haskell2010\n";
    }