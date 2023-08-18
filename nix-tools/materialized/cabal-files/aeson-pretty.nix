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
      identifier = { name = "aeson-pretty"; version = "0.8.10"; };
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
            (hsPkgs."attoparsec-aeson" or (errorHandler.buildDepError "attoparsec-aeson"))
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
      url = "http://hackage.haskell.org/package/aeson-pretty-0.8.10.tar.gz";
      sha256 = "2a21f2cd78adcb149ceba770239ed664519552911e7680172b18ff695cfa7ae5";
      });
    }) // {
    package-description-override = "cabal-version:  2.0\nname:           aeson-pretty\nversion:        0.8.10\nlicense:        BSD3\nlicense-file:   LICENSE\ncategory:       Text, Web, JSON, Pretty Printer\ncopyright:      Copyright 2011 Falko Peters\nauthor:         Falko Peters <falko.peters@gmail.com>\nmaintainer:     Martijn Bastiaan <martijn@hmbastiaan.nl>\nstability:      experimental\nhomepage:       http://github.com/informatikr/aeson-pretty\nbug-reports:    http://github.com/informatikr/aeson-pretty/issues\nbuild-type:     Simple\nsynopsis:       JSON pretty-printing library and command-line tool.\ndescription:\n    A JSON pretty-printing library compatible with aeson as well as\n    a command-line tool to improve readabilty of streams of JSON data.\n    .\n    The /library/ provides the function \"encodePretty\". It is a drop-in\n    replacement for aeson's \"encode\" function, producing JSON-ByteStrings for\n    human readers.\n    .\n    The /command-line tool/ reads JSON from stdin and writes prettified JSON\n    to stdout. It also offers a complementary \"compact\"-mode, essentially the\n    opposite of pretty-printing. If you specify @-flib-only@ like this\n    .\n        > cabal install -flib-only aeson-pretty\n    .\n    the command-line tool will NOT be installed.\n\nextra-source-files:\n    README.markdown\n    CHANGELOG.markdown\n\nflag lib-only\n    description: Only build/install the library, NOT the command-line tool.\n    default: False\n\nlibrary\n    exposed-modules:\n        Data.Aeson.Encode.Pretty\n\n    build-depends:\n        aeson ^>=1.1 || ^>=1.2 || ^>=1.3 || ^>=1.4 || ^>=1.5 || ^>=2.0 || ^>=2.1 || ^>=2.2,\n        base >= 4.5,\n        base-compat >= 0.9,\n        bytestring >= 0.9,\n        scientific >= 0.3,\n        vector >= 0.9,\n        text >= 0.11,\n        unordered-containers >= 0.2.14.0\n\n    if !impl(ghc >= 8.0)\n      build-depends:\n        semigroups >= 0.18.2\n\n    ghc-options: -Wall\n    default-language: Haskell2010\n\nexecutable aeson-pretty\n    hs-source-dirs: cli-tool\n    main-is: Main.hs\n    other-modules: Paths_aeson_pretty\n    autogen-modules: Paths_aeson_pretty\n\n    if flag(lib-only)\n        buildable: False\n    else\n        build-depends:\n            aeson >= 0.6,\n            aeson-pretty,\n            attoparsec >= 0.10,\n            attoparsec-aeson,\n            base == 4.*,\n            bytestring >= 0.9,\n            cmdargs >= 0.7\n\n    ghc-options: -Wall\n    ghc-prof-options: -auto-all\n    default-language: Haskell2010\n\nsource-repository head\n    type:     git\n    location: http://github.com/informatikr/aeson-pretty\n";
    }