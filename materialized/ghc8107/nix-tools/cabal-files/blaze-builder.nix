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
      identifier = { name = "blaze-builder"; version = "0.4.2.2"; };
      license = "BSD-3-Clause";
      copyright = "(c) 2010-2014 Simon Meier\n(c) 2010 Jasper Van der Jeugt\n(c) 2013-2015 Leon P Smith";
      maintainer = "https://github.com/blaze-builder";
      author = "Jasper Van der Jeugt, Simon Meier, Leon P Smith";
      homepage = "https://github.com/blaze-builder/blaze-builder";
      url = "";
      synopsis = "Efficient buffered output.";
      description = "This library allows to efficiently serialize Haskell values to lazy bytestrings\nwith a large average chunk size. The large average chunk size allows to make\ngood use of cache prefetching in later processing steps (e.g. compression) and\nreduces the system call overhead when writing the resulting lazy bytestring to a\nfile or sending it over the network.\n\nThis library was inspired by the module Data.Binary.Builder provided by the\nbinary package. It was originally developed with the specific needs of the\nblaze-html package in mind. Since then it has been restructured to serve as a\ndrop-in replacement for Data.Binary.Builder, which it improves upon both in\nspeed as well as expressivity.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = ([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          ] ++ (if compiler.isGhc && (compiler.version).lt "7.8"
          then [
            (hsPkgs."bytestring-builder" or (errorHandler.buildDepError "bytestring-builder"))
            ]
          else [
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            ])) ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).lt "8.0") (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"));
        buildable = true;
        };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."blaze-builder" or (errorHandler.buildDepError "blaze-builder"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."test-framework" or (errorHandler.buildDepError "test-framework"))
            (hsPkgs."test-framework-hunit" or (errorHandler.buildDepError "test-framework-hunit"))
            (hsPkgs."test-framework-quickcheck2" or (errorHandler.buildDepError "test-framework-quickcheck2"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."utf8-string" or (errorHandler.buildDepError "utf8-string"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/blaze-builder-0.4.2.2.tar.gz";
      sha256 = "2cdc998c021d3a5f2a66a95138b93386271c26a117e7676d78264a90e536af67";
      });
    }