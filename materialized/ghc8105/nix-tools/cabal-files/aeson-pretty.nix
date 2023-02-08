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
    flags = { lib-only = false; };
    package = {
      specVersion = "2.0";
      identifier = { name = "aeson-pretty"; version = "0.8.9"; };
      license = "BSD-3-Clause";
      copyright = "Copyright 2011 Falko Peters";
      maintainer = "Martijn Bastiaan <martijn@hmbastiaan.nl>";
      author = "Falko Peters <falko.peters@gmail.com>";
      homepage = "http://github.com/informatikr/aeson-pretty";
      url = "";
      synopsis = "JSON pretty-printing library and command-line tool.";
      description = "A JSON pretty-printing library compatible with aeson as well as\na command-line tool to improve readabilty of streams of JSON data.\n\nThe /library/ provides the function \"encodePretty\". It is a drop-in\nreplacement for aeson's \"encode\" function, producing JSON-ByteStrings for\nhuman readers.\n\nThe /command-line tool/ reads JSON from stdin and writes prettified JSON\nto stdout. It also offers a complementary \"compact\"-mode, essentially the\nopposite of pretty-printing. If you specify @-flib-only@ like this\n\n> cabal install -flib-only aeson-pretty\n\nthe command-line tool will NOT be installed.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."base-compat" or (errorHandler.buildDepError "base-compat"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          ] ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8.0")) (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"));
        buildable = true;
        };
      exes = {
        "aeson-pretty" = {
          depends = (pkgs.lib).optionals (!flags.lib-only) [
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."aeson-pretty" or (errorHandler.buildDepError "aeson-pretty"))
            (hsPkgs."attoparsec" or (errorHandler.buildDepError "attoparsec"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cmdargs" or (errorHandler.buildDepError "cmdargs"))
            ];
          buildable = if flags.lib-only then false else true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/aeson-pretty-0.8.9.tar.gz";
      sha256 = "5dbc4f451dfa1e667b2c6ec5170714fed1905dc9cae6a1134b3376f355fa2a08";
      });
    }) // {
    package-description-override = "cabal-version:  2.0\r\nname:           aeson-pretty\r\nversion:        0.8.9\r\nx-revision: 2\r\nlicense:        BSD3\r\nlicense-file:   LICENSE\r\ncategory:       Text, Web, JSON, Pretty Printer\r\ncopyright:      Copyright 2011 Falko Peters\r\nauthor:         Falko Peters <falko.peters@gmail.com>\r\nmaintainer:     Martijn Bastiaan <martijn@hmbastiaan.nl>\r\nstability:      experimental\r\nhomepage:       http://github.com/informatikr/aeson-pretty\r\nbug-reports:    http://github.com/informatikr/aeson-pretty/issues\r\nbuild-type:     Simple\r\nsynopsis:       JSON pretty-printing library and command-line tool.\r\ndescription:\r\n    A JSON pretty-printing library compatible with aeson as well as\r\n    a command-line tool to improve readabilty of streams of JSON data.\r\n    .\r\n    The /library/ provides the function \"encodePretty\". It is a drop-in\r\n    replacement for aeson's \"encode\" function, producing JSON-ByteStrings for\r\n    human readers.\r\n    .\r\n    The /command-line tool/ reads JSON from stdin and writes prettified JSON\r\n    to stdout. It also offers a complementary \"compact\"-mode, essentially the\r\n    opposite of pretty-printing. If you specify @-flib-only@ like this\r\n    .\r\n        > cabal install -flib-only aeson-pretty\r\n    .\r\n    the command-line tool will NOT be installed.\r\n\r\nextra-source-files:\r\n    README.markdown\r\n    CHANGELOG.markdown\r\n\r\nflag lib-only\r\n    description: Only build/install the library, NOT the command-line tool.\r\n    default: False\r\n\r\nlibrary\r\n    exposed-modules:\r\n        Data.Aeson.Encode.Pretty\r\n\r\n    build-depends:\r\n        aeson ^>= 1.0 || ^>=1.1 || ^>=1.2 || ^>=1.3 || ^>=1.4 || ^>=1.5 || ^>=2.0 || ^>=2.1,\r\n        base >= 4.5,\r\n        base-compat >= 0.9,\r\n        bytestring >= 0.9,\r\n        scientific >= 0.3,\r\n        vector >= 0.9,\r\n        text >= 0.11,\r\n        unordered-containers >= 0.2.14.0\r\n\r\n    if !impl(ghc >= 8.0)\r\n      build-depends:\r\n        semigroups >= 0.18.2\r\n\r\n    ghc-options: -Wall\r\n    default-language: Haskell2010\r\n\r\nexecutable aeson-pretty\r\n    hs-source-dirs: cli-tool\r\n    main-is: Main.hs\r\n    other-modules: Paths_aeson_pretty\r\n    autogen-modules: Paths_aeson_pretty\r\n\r\n    if flag(lib-only)\r\n        buildable: False\r\n    else\r\n        build-depends:\r\n            aeson >= 0.6,\r\n            aeson-pretty,\r\n            attoparsec >= 0.10,\r\n            base == 4.*,\r\n            bytestring >= 0.9,\r\n            cmdargs >= 0.7\r\n\r\n    ghc-options: -Wall\r\n    default-language: Haskell2010\r\n\r\nsource-repository head\r\n    type:     git\r\n    location: http://github.com/informatikr/aeson-pretty\r\n";
    }