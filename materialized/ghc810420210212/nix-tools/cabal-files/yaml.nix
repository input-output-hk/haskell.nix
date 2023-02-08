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
    flags = { no-examples = true; no-exe = true; };
    package = {
      specVersion = "1.12";
      identifier = { name = "yaml"; version = "0.11.8.0"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Michael Snoyman <michael@snoyman.com>";
      author = "Michael Snoyman <michael@snoyman.com>, Anton Ageev <antage@gmail.com>,Kirill Simonov";
      homepage = "https://github.com/snoyberg/yaml#readme";
      url = "";
      synopsis = "Support for parsing and rendering YAML documents.";
      description = "README and API documentation are available at <https://www.stackage.org/package/yaml>";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."attoparsec" or (errorHandler.buildDepError "attoparsec"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."conduit" or (errorHandler.buildDepError "conduit"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."libyaml" or (errorHandler.buildDepError "libyaml"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."resourcet" or (errorHandler.buildDepError "resourcet"))
          (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          ];
        buildable = true;
        };
      exes = {
        "examples" = {
          depends = [
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."attoparsec" or (errorHandler.buildDepError "attoparsec"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."conduit" or (errorHandler.buildDepError "conduit"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."libyaml" or (errorHandler.buildDepError "libyaml"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."resourcet" or (errorHandler.buildDepError "resourcet"))
            (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
            (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            ] ++ (pkgs.lib).optionals (!flags.no-examples) [
            (hsPkgs."raw-strings-qq" or (errorHandler.buildDepError "raw-strings-qq"))
            (hsPkgs."yaml" or (errorHandler.buildDepError "yaml"))
            ];
          buildable = if flags.no-examples then false else true;
          };
        "json2yaml" = {
          depends = [
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."attoparsec" or (errorHandler.buildDepError "attoparsec"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."conduit" or (errorHandler.buildDepError "conduit"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."libyaml" or (errorHandler.buildDepError "libyaml"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."resourcet" or (errorHandler.buildDepError "resourcet"))
            (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
            (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."yaml" or (errorHandler.buildDepError "yaml"))
            ];
          buildable = if flags.no-exe then false else true;
          };
        "yaml2json" = {
          depends = [
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."attoparsec" or (errorHandler.buildDepError "attoparsec"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."conduit" or (errorHandler.buildDepError "conduit"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."libyaml" or (errorHandler.buildDepError "libyaml"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."resourcet" or (errorHandler.buildDepError "resourcet"))
            (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
            (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."yaml" or (errorHandler.buildDepError "yaml"))
            ];
          buildable = if flags.no-exe then false else true;
          };
        };
      tests = {
        "spec" = {
          depends = [
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."attoparsec" or (errorHandler.buildDepError "attoparsec"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base-compat" or (errorHandler.buildDepError "base-compat"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."conduit" or (errorHandler.buildDepError "conduit"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."libyaml" or (errorHandler.buildDepError "libyaml"))
            (hsPkgs."mockery" or (errorHandler.buildDepError "mockery"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."raw-strings-qq" or (errorHandler.buildDepError "raw-strings-qq"))
            (hsPkgs."resourcet" or (errorHandler.buildDepError "resourcet"))
            (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
            (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
            (hsPkgs."temporary" or (errorHandler.buildDepError "temporary"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."yaml" or (errorHandler.buildDepError "yaml"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/yaml-0.11.8.0.tar.gz";
      sha256 = "f61a4e829bb75e17f5da39ea7b9d8d221a100a0f0cb1258bb9584a1829cd0ae8";
      });
    }) // {
    package-description-override = "cabal-version: 1.12\r\n\r\n-- This file has been generated from package.yaml by hpack version 0.34.4.\r\n--\r\n-- see: https://github.com/sol/hpack\r\n\r\nname:           yaml\r\nversion:        0.11.8.0\r\nx-revision: 2\r\nsynopsis:       Support for parsing and rendering YAML documents.\r\ndescription:    README and API documentation are available at <https://www.stackage.org/package/yaml>\r\ncategory:       Data\r\nstability:      stable\r\nhomepage:       https://github.com/snoyberg/yaml#readme\r\nbug-reports:    https://github.com/snoyberg/yaml/issues\r\nauthor:         Michael Snoyman <michael@snoyman.com>, Anton Ageev <antage@gmail.com>,Kirill Simonov\r\nmaintainer:     Michael Snoyman <michael@snoyman.com>\r\nlicense:        BSD3\r\nlicense-file:   LICENSE\r\nbuild-type:     Simple\r\nextra-source-files:\r\n    test/largest-string.yaml\r\n    test/json.yaml\r\n    test/resources/foo.yaml\r\n    test/resources/bar.yaml\r\n    test/resources/baz.yaml\r\n    test/resources/accent/foo.yaml\r\n    test/resources/loop/foo.yaml\r\n    test/resources/loop/bar.yaml\r\n    test/resources/empty.yaml\r\n    test/resources/empty2.yaml\r\n    README.md\r\n    ChangeLog.md\r\n\r\nsource-repository head\r\n  type: git\r\n  location: https://github.com/snoyberg/yaml\r\n\r\nflag no-examples\r\n  description: don't build the examples\r\n  manual: False\r\n  default: True\r\n\r\nflag no-exe\r\n  description: don't install the yaml2json or json2yaml executables\r\n  manual: False\r\n  default: True\r\n\r\nlibrary\r\n  exposed-modules:\r\n      Data.Yaml\r\n      Data.Yaml.Aeson\r\n      Data.Yaml.Builder\r\n      Data.Yaml.Config\r\n      Data.Yaml.Include\r\n      Data.Yaml.Internal\r\n      Data.Yaml.Parser\r\n      Data.Yaml.Pretty\r\n      Data.Yaml.TH\r\n  other-modules:\r\n      Paths_yaml\r\n  hs-source-dirs:\r\n      src\r\n  other-extensions:\r\n      LambdaCase\r\n  ghc-options: -Wall -Wcompat\r\n  build-depends:\r\n      aeson >=0.11\r\n    , attoparsec >=0.11.3.0\r\n    , base >=4.9.1 && <5\r\n    , bytestring >=0.9.1.4\r\n    , conduit >=1.2.8 && <1.4\r\n    , containers\r\n    , directory\r\n    , filepath\r\n    , libyaml ==0.1.*\r\n    , mtl\r\n    , resourcet >=0.3 && <1.4\r\n    , scientific >=0.3\r\n    , template-haskell\r\n    , text\r\n    , transformers >=0.1\r\n    , unordered-containers\r\n    , vector\r\n  default-language: Haskell2010\r\n\r\nexecutable examples\r\n  main-is: Main.hs\r\n  other-modules:\r\n      Config\r\n      Simple\r\n      Paths_yaml\r\n  hs-source-dirs:\r\n      examples\r\n  ghc-options: -Wall -Wcompat\r\n  build-depends:\r\n      aeson >=0.11\r\n    , attoparsec >=0.11.3.0\r\n    , base >=4.9.1 && <5\r\n    , bytestring >=0.9.1.4\r\n    , conduit >=1.2.8 && <1.4\r\n    , containers\r\n    , directory\r\n    , filepath\r\n    , libyaml ==0.1.*\r\n    , mtl\r\n    , resourcet >=0.3 && <1.4\r\n    , scientific >=0.3\r\n    , template-haskell\r\n    , text\r\n    , transformers >=0.1\r\n    , unordered-containers\r\n    , vector\r\n  if flag(no-examples)\r\n    buildable: False\r\n  else\r\n    build-depends:\r\n        raw-strings-qq\r\n      , yaml\r\n  default-language: Haskell2010\r\n\r\nexecutable json2yaml\r\n  main-is: json2yaml.hs\r\n  other-modules:\r\n      Common\r\n      Paths_yaml\r\n  hs-source-dirs:\r\n      exe\r\n  ghc-options: -Wall -Wcompat\r\n  build-depends:\r\n      aeson >=0.11\r\n    , attoparsec >=0.11.3.0\r\n    , base >=4.9.1 && <5\r\n    , bytestring >=0.9.1.4\r\n    , conduit >=1.2.8 && <1.4\r\n    , containers\r\n    , directory\r\n    , filepath\r\n    , libyaml ==0.1.*\r\n    , mtl\r\n    , optparse-applicative\r\n    , resourcet >=0.3 && <1.4\r\n    , scientific >=0.3\r\n    , template-haskell\r\n    , text\r\n    , transformers >=0.1\r\n    , unordered-containers\r\n    , vector\r\n    , yaml\r\n  if flag(no-exe)\r\n    buildable: False\r\n  default-language: Haskell2010\r\n\r\nexecutable yaml2json\r\n  main-is: yaml2json.hs\r\n  other-modules:\r\n      Common\r\n      Paths_yaml\r\n  hs-source-dirs:\r\n      exe\r\n  other-extensions:\r\n      CPP\r\n      LambdaCase\r\n      NamedFieldPuns\r\n  ghc-options: -Wall -Wcompat\r\n  build-depends:\r\n      aeson >=0.11\r\n    , attoparsec >=0.11.3.0\r\n    , base >=4.9.1 && <5\r\n    , bytestring >=0.9.1.4\r\n    , conduit >=1.2.8 && <1.4\r\n    , containers\r\n    , directory\r\n    , filepath\r\n    , libyaml ==0.1.*\r\n    , mtl\r\n    , optparse-applicative\r\n    , resourcet >=0.3 && <1.4\r\n    , scientific >=0.3\r\n    , template-haskell\r\n    , text\r\n    , transformers >=0.1\r\n    , unordered-containers\r\n    , vector\r\n    , yaml\r\n  if flag(no-exe)\r\n    buildable: False\r\n  default-language: Haskell2010\r\n\r\ntest-suite spec\r\n  type: exitcode-stdio-1.0\r\n  main-is: Spec.hs\r\n  other-modules:\r\n      Data.Yaml.IncludeSpec\r\n      Data.Yaml.THSpec\r\n      Data.YamlSpec\r\n      Paths_yaml\r\n  hs-source-dirs:\r\n      test\r\n  ghc-options: -Wall -Wcompat -with-rtsopts=-K1K\r\n  cpp-options: -DTEST\r\n  build-depends:\r\n      HUnit\r\n    , aeson >=0.11\r\n    , attoparsec >=0.11.3.0\r\n    , base >=4.9.1 && <5\r\n    , base-compat\r\n    , bytestring >=0.9.1.4\r\n    , conduit >=1.2.8 && <1.4\r\n    , containers\r\n    , directory\r\n    , filepath\r\n    , hspec >=1.3\r\n    , libyaml ==0.1.*\r\n    , mockery\r\n    , mtl\r\n    , raw-strings-qq\r\n    , resourcet >=0.3 && <1.4\r\n    , scientific >=0.3\r\n    , template-haskell\r\n    , temporary\r\n    , text\r\n    , transformers >=0.1\r\n    , unordered-containers\r\n    , vector\r\n    , yaml\r\n  default-language: Haskell2010\r\n";
    }