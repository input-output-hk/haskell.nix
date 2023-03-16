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
      identifier = { name = "js-flot"; version = "0.8.3"; };
      license = "MIT";
      copyright = "Neil Mitchell 2014";
      maintainer = "Neil Mitchell <ndmitchell@gmail.com>";
      author = "Neil Mitchell <ndmitchell@gmail.com>";
      homepage = "https://github.com/ndmitchell/js-flot#readme";
      url = "";
      synopsis = "Obtain minified flot code";
      description = "This package bundles the minified <http://www.flotcharts.org/ Flot> code\n(a jQuery plotting library) into a Haskell package,\nso it can be depended upon by Cabal packages. The first three components of\nthe version number match the upstream flot version. The package is designed\nto meet the redistribution requirements of downstream users (e.g. Debian).";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [ (hsPkgs."base" or (errorHandler.buildDepError "base")) ];
        buildable = true;
        };
      tests = {
        "js-flot-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."HTTP" or (errorHandler.buildDepError "HTTP"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/js-flot-0.8.3.tar.gz";
      sha256 = "1ba2f2a6b8d85da76c41f526c98903cbb107f8642e506c072c1e7e3c20fe5e7a";
      });
    }) // {
    package-description-override = "cabal-version:      >= 1.10\nbuild-type:         Simple\nname:               js-flot\nversion:            0.8.3\nlicense:            MIT\nlicense-file:       LICENSE\ncategory:           Javascript\nauthor:             Neil Mitchell <ndmitchell@gmail.com>\nmaintainer:         Neil Mitchell <ndmitchell@gmail.com>\ncopyright:          Neil Mitchell 2014\nsynopsis:           Obtain minified flot code\ndescription:\n    This package bundles the minified <http://www.flotcharts.org/ Flot> code\n    (a jQuery plotting library) into a Haskell package,\n    so it can be depended upon by Cabal packages. The first three components of\n    the version number match the upstream flot version. The package is designed\n    to meet the redistribution requirements of downstream users (e.g. Debian).\nhomepage:           https://github.com/ndmitchell/js-flot#readme\nbug-reports:        https://github.com/ndmitchell/js-flot/issues\ntested-with:        GHC==7.8.3, GHC==7.6.3, GHC==7.4.2, GHC==7.2.2\nextra-source-files:\n    javascript/flot-0.8.3.zip\n    CHANGES.txt\n    README.md\n\ndata-dir: javascript\ndata-files:\n    jquery.flot.min.js\n    jquery.flot.canvas.min.js\n    jquery.flot.categories.min.js\n    jquery.flot.crosshair.min.js\n    jquery.flot.errorbars.min.js\n    jquery.flot.fillbetween.min.js\n    jquery.flot.image.min.js\n    jquery.flot.navigate.min.js\n    jquery.flot.pie.min.js\n    jquery.flot.resize.min.js\n    jquery.flot.selection.min.js\n    jquery.flot.stack.min.js\n    jquery.flot.symbol.min.js\n    jquery.flot.threshold.min.js\n    jquery.flot.time.min.js\n\nsource-repository head\n    type:     git\n    location: https://github.com/ndmitchell/js-flot.git\n\nlibrary\n    default-language: Haskell2010\n    build-depends:\n        base == 4.*\n\n    exposed-modules:\n        Language.Javascript.Flot\n\n    other-modules:\n        Paths_js_flot\n\ntest-suite js-flot-test\n    default-language: Haskell2010\n    type: exitcode-stdio-1.0\n    main-is: Test.hs\n    build-depends:\n        base == 4.*,\n        HTTP\n";
    }