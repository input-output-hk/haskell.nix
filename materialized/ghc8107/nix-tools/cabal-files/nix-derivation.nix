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
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = { name = "nix-derivation"; version = "1.1.2"; };
      license = "BSD-3-Clause";
      copyright = "2017 Gabriella Gonzalez";
      maintainer = "GenuineGabriella@gmail.com";
      author = "Gabriella Gonzalez";
      homepage = "";
      url = "";
      synopsis = "Parse and render *.drv files";
      description = "Use this package to parse and render Nix derivation files (i.e. *.drv files)\n\nThis package also provides a @pretty-derivation@ executable which reads a\nderivation on standard input and outputs the pretty-printed Haskell\nrepresentation on standard output";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."attoparsec" or (errorHandler.buildDepError "attoparsec"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          ];
        buildable = true;
        };
      exes = {
        "pretty-derivation" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."attoparsec" or (errorHandler.buildDepError "attoparsec"))
            (hsPkgs."pretty-show" or (errorHandler.buildDepError "pretty-show"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."nix-derivation" or (errorHandler.buildDepError "nix-derivation"))
            ];
          buildable = true;
          };
        };
      tests = {
        "example" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."attoparsec" or (errorHandler.buildDepError "attoparsec"))
            (hsPkgs."nix-derivation" or (errorHandler.buildDepError "nix-derivation"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            ];
          buildable = true;
          };
        "property" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."attoparsec" or (errorHandler.buildDepError "attoparsec"))
            (hsPkgs."nix-derivation" or (errorHandler.buildDepError "nix-derivation"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            ];
          buildable = true;
          };
        };
      benchmarks = {
        "benchmark" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."attoparsec" or (errorHandler.buildDepError "attoparsec"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."nix-derivation" or (errorHandler.buildDepError "nix-derivation"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/nix-derivation-1.1.2.tar.gz";
      sha256 = "c7ff162f245021d7ba8ea24b993b1df2241744f6e0a78b0783092182fbea8808";
      });
    }