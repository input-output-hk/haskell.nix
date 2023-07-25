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
      specVersion = "1.0";
      identifier = { name = "monadlist"; version = "0.0.2"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "thomasedingcode@gmail.com";
      author = "Thomas Eding";
      homepage = "";
      url = "";
      synopsis = "Monadic versions of list functions";
      description = "Monadic versions of list functions seen in Data.List.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [ (hsPkgs."base" or (errorHandler.buildDepError "base")) ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/monadlist-0.0.2.tar.gz";
      sha256 = "06bbe82c9fc2a35048788367da74bb5f79c7e6be2ae38eca20f332f8cbc5fdfe";
      });
    }) // {
    package-description-override = "name: monadlist\nversion: 0.0.2\ncabal-version: -any\nbuild-type: Simple\nlicense: BSD3\nlicense-file: LICENSE\ncopyright:\nmaintainer: thomasedingcode@gmail.com\nbuild-depends: base >=3 && <5\nstability:\nhomepage:\npackage-url:\nbug-reports:\nsynopsis: Monadic versions of list functions\ndescription: Monadic versions of list functions seen in Data.List.\ncategory: Control\nauthor: Thomas Eding\ntested-with:\ndata-files:\ndata-dir: \"\"\nextra-source-files:\nextra-tmp-files:\nexposed-modules: Control.Monad.ListM\nexposed: True\nbuildable: True\nbuild-tools:\ncpp-options:\ncc-options:\nld-options:\npkgconfig-depends:\nframeworks:\nc-sources:\ndefault-language:\nother-languages:\ndefault-extensions:\nother-extensions:\nextensions:\nextra-libraries:\nextra-lib-dirs:\nincludes:\ninstall-includes:\ninclude-dirs:\nhs-source-dirs: src\nother-modules:\nghc-prof-options:\nghc-shared-options:\nghc-options:\nhugs-options:\nnhc98-options:\njhc-options:";
    }