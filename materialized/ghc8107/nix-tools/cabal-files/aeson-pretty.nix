{ system
  , compiler
  , flags
  , pkgs
  , hsPkgs
  , pkgconfPkgs
  , errorHandler
  , config
  , ... }:
  {
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
    }