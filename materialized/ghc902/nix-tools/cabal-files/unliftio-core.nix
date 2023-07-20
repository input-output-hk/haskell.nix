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
      specVersion = "1.12";
      identifier = { name = "unliftio-core"; version = "0.2.1.0"; };
      license = "MIT";
      copyright = "2017-2020 FP Complete";
      maintainer = "michael@snoyman.com";
      author = "Michael Snoyman, Francesco Mazzoli";
      homepage = "https://github.com/fpco/unliftio/tree/master/unliftio-core#readme";
      url = "";
      synopsis = "The MonadUnliftIO typeclass for unlifting monads to IO";
      description = "Please see the documentation and README at <https://www.stackage.org/package/unliftio-core>";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/unliftio-core-0.2.1.0.tar.gz";
      sha256 = "99384cba8d56d9d61b85e38a313a93ebcdb78be6566367f0930ef580597fe3e3";
      });
    }) // {
    package-description-override = "cabal-version: 1.12\r\n\r\n-- This file has been generated from package.yaml by hpack version 0.35.1.\r\n--\r\n-- see: https://github.com/sol/hpack\r\n\r\nname:           unliftio-core\r\nversion:        0.2.1.0\r\nx-revision: 2\r\nsynopsis:       The MonadUnliftIO typeclass for unlifting monads to IO\r\ndescription:    Please see the documentation and README at <https://www.stackage.org/package/unliftio-core>\r\ncategory:       Control\r\nhomepage:       https://github.com/fpco/unliftio/tree/master/unliftio-core#readme\r\nauthor:         Michael Snoyman, Francesco Mazzoli\r\nmaintainer:     michael@snoyman.com\r\ncopyright:      2017-2020 FP Complete\r\nlicense:        MIT\r\nlicense-file:   LICENSE\r\nbuild-type:     Simple\r\nextra-source-files:\r\n    README.md\r\n    ChangeLog.md\r\n\r\nlibrary\r\n  exposed-modules:\r\n      Control.Monad.IO.Unlift\r\n  other-modules:\r\n      Paths_unliftio_core\r\n  hs-source-dirs:\r\n      src\r\n  build-depends:\r\n      base >=4.9 && <4.19\r\n    , transformers >=0.2 && <0.7\r\n  default-language: Haskell2010\r\n";
    }