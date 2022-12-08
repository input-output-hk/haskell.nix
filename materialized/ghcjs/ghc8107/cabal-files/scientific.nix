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
    flags = { bytestring-builder = false; integer-simple = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "scientific"; version = "0.3.6.2"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Bas van Dijk <v.dijk.bas@gmail.com>";
      author = "Bas van Dijk";
      homepage = "https://github.com/basvandijk/scientific";
      url = "";
      synopsis = "Numbers represented using scientific notation";
      description = "\"Data.Scientific\" provides the number type 'Scientific'. Scientific numbers are\narbitrary precision and space efficient. They are represented using\n<http://en.wikipedia.org/wiki/Scientific_notation scientific notation>.\nThe implementation uses a coefficient @c :: 'Integer'@ and a base-10 exponent\n@e :: 'Int'@. A scientific number corresponds to the\n'Fractional' number: @'fromInteger' c * 10 '^^' e@.\n\nNote that since we're using an 'Int' to represent the exponent these numbers\naren't truly arbitrary precision. I intend to change the type of the exponent\nto 'Integer' in a future release.\n\nThe main application of 'Scientific' is to be used as the target of parsing\narbitrary precision numbers coming from an untrusted source. The advantages\nover using 'Rational' for this are that:\n\n* A 'Scientific' is more efficient to construct. Rational numbers need to be\nconstructed using '%' which has to compute the 'gcd' of the 'numerator' and\n'denominator'.\n\n* 'Scientific' is safe against numbers with huge exponents. For example:\n@1e1000000000 :: 'Rational'@ will fill up all space and crash your\nprogram. Scientific works as expected:\n\n>>> read \"1e1000000000\" :: Scientific\n1.0e1000000000\n\n* Also, the space usage of converting scientific numbers with huge exponents to\n@'Integral's@ (like: 'Int') or @'RealFloat's@ (like: 'Double' or 'Float')\nwill always be bounded by the target type.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = ([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."integer-logarithms" or (errorHandler.buildDepError "integer-logarithms"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
          ] ++ (if flags.bytestring-builder
          then [
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."bytestring-builder" or (errorHandler.buildDepError "bytestring-builder"))
            ]
          else [
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            ])) ++ (if flags.integer-simple
          then [
            (hsPkgs."integer-simple" or (errorHandler.buildDepError "integer-simple"))
            ]
          else [
            (hsPkgs."integer-gmp" or (errorHandler.buildDepError "integer-gmp"))
            ]);
        buildable = true;
        };
      tests = {
        "test-scientific" = {
          depends = [
            (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-ant-xml" or (errorHandler.buildDepError "tasty-ant-xml"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-smallcheck" or (errorHandler.buildDepError "tasty-smallcheck"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."smallcheck" or (errorHandler.buildDepError "smallcheck"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            ] ++ (if flags.bytestring-builder
            then [
              (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
              (hsPkgs."bytestring-builder" or (errorHandler.buildDepError "bytestring-builder"))
              ]
            else [
              (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
              ]);
          buildable = true;
          };
        };
      benchmarks = {
        "bench-scientific" = {
          depends = [
            (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/scientific-0.3.6.2.tar.gz";
      sha256 = "278d0afc87450254f8a76eab21b5583af63954efc9b74844a17a21a68013140f";
      });
    }