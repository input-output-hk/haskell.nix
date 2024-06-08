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
      identifier = { name = "js-jquery"; version = "3.3.1"; };
      license = "MIT";
      copyright = "Neil Mitchell 2014-2018";
      maintainer = "Neil Mitchell <ndmitchell@gmail.com>";
      author = "Neil Mitchell <ndmitchell@gmail.com>";
      homepage = "https://github.com/ndmitchell/js-jquery#readme";
      url = "";
      synopsis = "Obtain minified jQuery code";
      description = "This package bundles the minified <http://jquery.com/ jQuery> code into a Haskell package,\nso it can be depended upon by Cabal packages. The first three components of\nthe version number match the upstream jQuery version. The package is designed\nto meet the redistribution requirements of downstream users (e.g. Debian).";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [ (hsPkgs."base" or (errorHandler.buildDepError "base")) ];
        buildable = true;
      };
      tests = {
        "js-jquery-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."js-jquery" or (errorHandler.buildDepError "js-jquery"))
            (hsPkgs."HTTP" or (errorHandler.buildDepError "HTTP"))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/js-jquery-3.3.1.tar.gz";
      sha256 = "e0e0681f0da1130ede4e03a051630ea439c458cb97216cdb01771ebdbe44069b";
    });
  }) // {
    package-description-override = "cabal-version:      >= 1.18\nbuild-type:         Simple\nname:               js-jquery\nversion:            3.3.1\nlicense:            MIT\nlicense-file:       LICENSE\ncategory:           Javascript\nauthor:             Neil Mitchell <ndmitchell@gmail.com>\nmaintainer:         Neil Mitchell <ndmitchell@gmail.com>\ncopyright:          Neil Mitchell 2014-2018\nsynopsis:           Obtain minified jQuery code\ndescription:\n    This package bundles the minified <http://jquery.com/ jQuery> code into a Haskell package,\n    so it can be depended upon by Cabal packages. The first three components of\n    the version number match the upstream jQuery version. The package is designed\n    to meet the redistribution requirements of downstream users (e.g. Debian).\nhomepage:           https://github.com/ndmitchell/js-jquery#readme\nbug-reports:        https://github.com/ndmitchell/js-jquery/issues\ntested-with:        GHC==8.2.2, GHC==8.0.2, GHC==7.10.3, GHC==7.8.4, GHC==7.6.3, GHC==7.4.2\nextra-source-files:\n    javascript/jquery-3.3.1.js\nextra-doc-files:\n    CHANGES.txt\n    README.md\n\ndata-dir: javascript\ndata-files:\n    jquery-3.3.1.min.js\n\nsource-repository head\n    type:     git\n    location: https://github.com/ndmitchell/js-jquery.git\n\nlibrary\n    default-language: Haskell2010\n    hs-source-dirs:   src\n    build-depends:\n        base == 4.*\n\n    exposed-modules:\n        Language.Javascript.JQuery\n\n    other-modules:\n        Paths_js_jquery\n\ntest-suite js-jquery-test\n    default-language: Haskell2010\n    type: exitcode-stdio-1.0\n    main-is: src/Test.hs\n    other-modules:\n        Paths_js_jquery\n    build-depends:\n        base == 4.*,\n        js-jquery,\n        HTTP\n";
  }