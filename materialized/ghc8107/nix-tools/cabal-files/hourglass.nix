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
      identifier = { name = "hourglass"; version = "0.2.12"; };
      license = "BSD-3-Clause";
      copyright = "Vincent Hanquez <vincent@snarc.org>";
      maintainer = "vincent@snarc.org";
      author = "Vincent Hanquez <vincent@snarc.org>";
      homepage = "https://github.com/vincenthz/hs-hourglass";
      url = "";
      synopsis = "simple performant time related library";
      description = "Simple time library focusing on simple but powerful and performant API\n\nThe backbone of the library are the Timeable and Time type classes.\n\nEach Timeable instances can be converted to type that has a Time instances,\nand thus are different representations of current time.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          ] ++ (pkgs.lib).optional (system.isWindows) (hsPkgs."Win32" or (errorHandler.buildDepError "Win32"));
        buildable = true;
        };
      tests = {
        "test-hourglass" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."hourglass" or (errorHandler.buildDepError "hourglass"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."old-locale" or (errorHandler.buildDepError "old-locale"))
            ];
          buildable = true;
          };
        };
      benchmarks = {
        "bench-hourglass" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."gauge" or (errorHandler.buildDepError "gauge"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."hourglass" or (errorHandler.buildDepError "hourglass"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."old-locale" or (errorHandler.buildDepError "old-locale"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/hourglass-0.2.12.tar.gz";
      sha256 = "44335b5c402e80c60f1db6a74462be4ea29d1a9043aa994334ffee1164f1ca4a";
      });
    }