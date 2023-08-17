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
      identifier = { name = "http-client-tls"; version = "0.3.6.1"; };
      license = "MIT";
      copyright = "";
      maintainer = "michael@snoyman.com";
      author = "Michael Snoyman";
      homepage = "https://github.com/snoyberg/http-client";
      url = "";
      synopsis = "http-client backend using the connection package and tls library";
      description = "Hackage documentation generation is not reliable. For up to date documentation, please see: <https://www.stackage.org/package/http-client-tls>.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."data-default-class" or (errorHandler.buildDepError "data-default-class"))
          (hsPkgs."http-client" or (errorHandler.buildDepError "http-client"))
          (hsPkgs."connection" or (errorHandler.buildDepError "connection"))
          (hsPkgs."network" or (errorHandler.buildDepError "network"))
          (hsPkgs."tls" or (errorHandler.buildDepError "tls"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."case-insensitive" or (errorHandler.buildDepError "case-insensitive"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."http-types" or (errorHandler.buildDepError "http-types"))
          (hsPkgs."cryptonite" or (errorHandler.buildDepError "cryptonite"))
          (hsPkgs."memory" or (errorHandler.buildDepError "memory"))
          (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."network-uri" or (errorHandler.buildDepError "network-uri"))
          ];
        buildable = true;
        };
      tests = {
        "spec" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."connection" or (errorHandler.buildDepError "connection"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."http-client" or (errorHandler.buildDepError "http-client"))
            (hsPkgs."http-client-tls" or (errorHandler.buildDepError "http-client-tls"))
            (hsPkgs."http-types" or (errorHandler.buildDepError "http-types"))
            ];
          buildable = true;
          };
        };
      benchmarks = {
        "benchmark" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."gauge" or (errorHandler.buildDepError "gauge"))
            (hsPkgs."http-client" or (errorHandler.buildDepError "http-client"))
            (hsPkgs."http-client-tls" or (errorHandler.buildDepError "http-client-tls"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/http-client-tls-0.3.6.1.tar.gz";
      sha256 = "b19fff86a41b6035cbd97271a5d6965e43dcc4bedbe4c03dd586fed65fbac80d";
      });
    }) // {
    package-description-override = "name:                http-client-tls\nversion:             0.3.6.1\nsynopsis:            http-client backend using the connection package and tls library\ndescription:         Hackage documentation generation is not reliable. For up to date documentation, please see: <https://www.stackage.org/package/http-client-tls>.\nhomepage:            https://github.com/snoyberg/http-client\nlicense:             MIT\nlicense-file:        LICENSE\nauthor:              Michael Snoyman\nmaintainer:          michael@snoyman.com\ncategory:            Network\nbuild-type:          Simple\ncabal-version:       >=1.10\nextra-source-files:  README.md\n                     ChangeLog.md\n\nlibrary\n  exposed-modules:     Network.HTTP.Client.TLS\n  other-extensions:    ScopedTypeVariables\n  build-depends:       base >= 4.10 && < 5\n                     , data-default-class\n                     , http-client >= 0.7.11\n                     , connection >= 0.2.5\n                     , network\n                     , tls >= 1.2\n                     , bytestring\n                     , case-insensitive\n                     , transformers\n                     , http-types\n                     , cryptonite\n                     , memory\n                     , exceptions\n                     , containers\n                     , text\n                     , network-uri\n  default-language:    Haskell2010\n  ghc-options:         -Wall\n\ntest-suite spec\n  main-is:             Spec.hs\n  type:                exitcode-stdio-1.0\n  hs-source-dirs:      test\n  default-language:    Haskell2010\n  build-depends:       base\n                     , connection\n                     , hspec\n                     , http-client\n                     , http-client-tls\n                     , http-types\n\nbenchmark benchmark\n  main-is:             Bench.hs\n  type:                exitcode-stdio-1.0\n  hs-source-dirs:      bench\n  default-language:    Haskell2010\n  build-depends:       base\n                     , gauge\n                     , http-client\n                     , http-client-tls\n";
    }