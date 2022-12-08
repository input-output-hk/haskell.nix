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
    flags = { buildreadme = false; text = true; };
    package = {
      specVersion = "1.10";
      identifier = { name = "prettyprinter"; version = "1.7.1"; };
      license = "BSD-2-Clause";
      copyright = "";
      maintainer = "Simon Jakobi <simon.jakobi@gmail.com>, David Luposchainsky <dluposchainsky at google>";
      author = "Phil Wadler, Daan Leijen, Max Bolingbroke, Edward Kmett, David Luposchainsky, Simon Jakobi";
      homepage = "http://github.com/quchen/prettyprinter";
      url = "";
      synopsis = "A modern, easy to use, well-documented, extensible pretty-printer.";
      description = "A modern, easy to use, well-documented, extensible pretty-printer. For more see README.md";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = ((([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          ] ++ (pkgs.lib).optional (flags.text) (hsPkgs."text" or (errorHandler.buildDepError "text"))) ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "7.6")) (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))) ++ (pkgs.lib).optionals (!(compiler.isGhc && (compiler.version).ge "8.0")) [
          (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
          (hsPkgs."fail" or (errorHandler.buildDepError "fail"))
          ]) ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "7.10")) (hsPkgs."void" or (errorHandler.buildDepError "void"));
        buildable = true;
        };
      exes = {
        "generate_readme" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
            ];
          buildable = if flags.buildreadme && flags.text then true else false;
          };
        };
      tests = {
        "doctest" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."doctest" or (errorHandler.buildDepError "doctest"))
            (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            ];
          buildable = if compiler.isGhc && (compiler.version).lt "7.10"
            then false
            else true;
          };
        "testsuite" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
            (hsPkgs."pgp-wordlist" or (errorHandler.buildDepError "pgp-wordlist"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."quickcheck-instances" or (errorHandler.buildDepError "quickcheck-instances"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            ] ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8.0")) (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"));
          buildable = if !flags.text then false else true;
          };
        };
      benchmarks = {
        "fusion" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
            (hsPkgs."gauge" or (errorHandler.buildDepError "gauge"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."ansi-wl-pprint" or (errorHandler.buildDepError "ansi-wl-pprint"))
            ];
          buildable = if !flags.text then false else true;
          };
        "faster-unsafe-text" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
            (hsPkgs."gauge" or (errorHandler.buildDepError "gauge"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            ];
          buildable = if !flags.text then false else true;
          };
        "large-output" = {
          depends = ([
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base-compat" or (errorHandler.buildDepError "base-compat"))
            (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
            (hsPkgs."ansi-wl-pprint" or (errorHandler.buildDepError "ansi-wl-pprint"))
            (hsPkgs."gauge" or (errorHandler.buildDepError "gauge"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            ] ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "7.6")) (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))) ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8.0")) (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"));
          buildable = if !flags.text then false else true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/prettyprinter-1.7.1.tar.gz";
      sha256 = "5e6ea6903114fa118fcc359633dfb7ecddecb92c06c853d02a77b72b251f0b45";
      });
    }