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
      identifier = { name = "ref-tf"; version = "0.5.0.1"; };
      license = "BSD-3-Clause";
      copyright = "(c) 2006-2011 Harvard University\n(c) 2011-2021 Geoffrey Mainland";
      maintainer = "Geoffrey Mainland <mainland@drexel.edu>";
      author = "Geoffrey Mainland <mainland@drexel.edu>";
      homepage = "";
      url = "";
      synopsis = "A type class for monads with references using type families.";
      description = "Contains a 'MonadRef' type class that abstracts over the\ndetails of manipulating references, allowing one to write code\nthat can operate in either the ST monad or the IO monad.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/ref-tf-0.5.0.1.tar.gz";
      sha256 = "a5c3243c30119be1cc53d04fddc3f3a3969c72912a52a111f45f04bed8a35147";
      });
    }) // {
    package-description-override = "name:           ref-tf\nversion:        0.5.0.1\ncabal-version:  >= 1.10\nlicense:        BSD3\nlicense-file:   LICENSE\ncopyright:      (c) 2006-2011 Harvard University\n                (c) 2011-2021 Geoffrey Mainland\nauthor:         Geoffrey Mainland <mainland@drexel.edu>\nmaintainer:     Geoffrey Mainland <mainland@drexel.edu>\nstability:      alpha\nbug-reports:    https://github.com/mainland/ref-tf/issues\ncategory:       Control\nsynopsis:       A type class for monads with references using type families.\ndescription:    Contains a 'MonadRef' type class that abstracts over the\n                details of manipulating references, allowing one to write code\n                that can operate in either the ST monad or the IO monad.\ntested-with:    GHC==7.4.2, GHC==7.6.3, GHC==7.8.4, GHC==7.10.3, GHC==8.0.2,\n                GHC==8.2.2, GHC==8.4.3, GHC==8.6.5, GHC==8.8.4, GHC==8.10.7,\n                GHC==9.0.1, GHC==9.2.1\n\nbuild-type:     Simple\n\nlibrary\n  default-language: Haskell2010\n\n  exposed-modules:\n    Control.Monad.Ref\n\n  build-depends:\n    base         >= 4   && < 5,\n    stm          >= 2.1 && < 2.6,\n    transformers >= 0.2 && < 0.7\n\nsource-repository head\n  type:     git\n  location: git://github.com/mainland/ref-tf.git\n";
    }