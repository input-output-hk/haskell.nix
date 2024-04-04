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
      specVersion = "1.18";
      identifier = { name = "js-dgtable"; version = "0.5.2"; };
      license = "MIT";
      copyright = "Neil Mitchell 2019";
      maintainer = "Neil Mitchell <ndmitchell@gmail.com>";
      author = "Neil Mitchell <ndmitchell@gmail.com>";
      homepage = "https://github.com/ndmitchell/js-dgtable#readme";
      url = "";
      synopsis = "Obtain minified jquery.dgtable code";
      description = "This package bundles the minified <https://github.com/danielgindi/jquery.dgtable jquery.dgtable> code into a Haskell package,\nso it can be depended upon by Cabal packages. The first three components of\nthe version number match the upstream jquery.dgtable version. The package is designed\nto meet the redistribution requirements of downstream users (e.g. Debian).";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [ (hsPkgs."base" or (errorHandler.buildDepError "base")) ];
        buildable = true;
        };
      tests = {
        "js-dgtable-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."js-dgtable" or (errorHandler.buildDepError "js-dgtable"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/js-dgtable-0.5.2.tar.gz";
      sha256 = "e28dd65bee8083b17210134e22e01c6349dc33c3b7bd17705973cd014e9f20ac";
      });
    }) // {
    package-description-override = "cabal-version:      >= 1.18\nbuild-type:         Simple\nname:               js-dgtable\nversion:            0.5.2\nlicense:            MIT\nlicense-file:       LICENSE\ncategory:           Javascript\nauthor:             Neil Mitchell <ndmitchell@gmail.com>\nmaintainer:         Neil Mitchell <ndmitchell@gmail.com>\ncopyright:          Neil Mitchell 2019\nsynopsis:           Obtain minified jquery.dgtable code\ndescription:\n    This package bundles the minified <https://github.com/danielgindi/jquery.dgtable jquery.dgtable> code into a Haskell package,\n    so it can be depended upon by Cabal packages. The first three components of\n    the version number match the upstream jquery.dgtable version. The package is designed\n    to meet the redistribution requirements of downstream users (e.g. Debian).\nhomepage:           https://github.com/ndmitchell/js-dgtable#readme\nbug-reports:        https://github.com/ndmitchell/js-dgtable/issues\ntested-with:        GHC==8.6.4, GHC==8.4.4, GHC==8.2.2, GHC==8.0.2, GHC==7.10.3\nextra-source-files:\n    javascript/jquery.dgtable.js\nextra-doc-files:\n    CHANGES.txt\n    README.md\n\ndata-dir: javascript\ndata-files:\n    jquery.dgtable.min.js\n\nsource-repository head\n    type:     git\n    location: https://github.com/ndmitchell/js-dgtable.git\n\nlibrary\n    default-language: Haskell2010\n    hs-source-dirs:   src\n    build-depends:\n        base == 4.*\n\n    exposed-modules:\n        Language.Javascript.DGTable\n\n    other-modules:\n        Paths_js_dgtable\n\ntest-suite js-dgtable-test\n    default-language: Haskell2010\n    type: exitcode-stdio-1.0\n    main-is: src/Test.hs\n    other-modules:\n        Paths_js_dgtable\n    build-depends:\n        base == 4.*,\n        js-dgtable\n";
    }