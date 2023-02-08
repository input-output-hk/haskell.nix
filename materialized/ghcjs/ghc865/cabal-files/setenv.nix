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
      specVersion = "1.8";
      identifier = { name = "setenv"; version = "0.1.1.3"; };
      license = "MIT";
      copyright = "(c) 2012-2015 Simon Hengel";
      maintainer = "Simon Hengel <sol@typeful.net>";
      author = "Simon Hengel <sol@typeful.net>";
      homepage = "";
      url = "";
      synopsis = "A cross-platform library for setting environment variables";
      description = "A cross-platform library for setting environment variables\n\nNote: Since @base-4.7.0.0@ the functions @setEnv@ and\n@unsetEnv@ are provided by @System.Environment@.\n@System.SetEnv@ merily re-exports those functions when built\nwith @base >= 4.7@.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          ] ++ (pkgs.lib).optional (!system.isWindows) (hsPkgs."unix" or (errorHandler.buildDepError "unix"));
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/setenv-0.1.1.3.tar.gz";
      sha256 = "e358df39afc03d5a39e2ec650652d845c85c80cc98fe331654deafb4767ecb32";
      });
    }) // {
    package-description-override = "name:             setenv\r\nversion:          0.1.1.3\r\nx-revision: 1\r\nlicense:          MIT\r\nlicense-file:     LICENSE\r\ncopyright:        (c) 2012-2015 Simon Hengel\r\nauthor:           Simon Hengel <sol@typeful.net>\r\nmaintainer:       Simon Hengel <sol@typeful.net>\r\ncategory:         System\r\nsynopsis:         A cross-platform library for setting environment variables\r\ndescription:      A cross-platform library for setting environment variables\r\n                  .\r\n                  Note: Since @base-4.7.0.0@ the functions @setEnv@ and\r\n                  @unsetEnv@ are provided by @System.Environment@.\r\n                  @System.SetEnv@ merily re-exports those functions when built\r\n                  with @base >= 4.7@.\r\nbuild-type:       Simple\r\ncabal-version:    >= 1.8\r\n\r\nsource-repository head\r\n  type: git\r\n  location: https://github.com/sol/setenv\r\n\r\nlibrary\r\n  ghc-options:\r\n      -Wall\r\n  hs-source-dirs:\r\n      src\r\n  exposed-modules:\r\n      System.SetEnv\r\n  build-depends:\r\n      base == 4.*\r\n  if !os(windows)\r\n    build-depends: unix\r\n";
    }