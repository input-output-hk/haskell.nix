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
      identifier = { name = "base-compat"; version = "0.11.2"; };
      license = "MIT";
      copyright = "(c) 2012-2018 Simon Hengel,\n(c) 2014-2018 João Cristóvão,\n(c) 2015-2018 Ryan Scott";
      maintainer = "Simon Hengel <sol@typeful.net>,\nJoão Cristóvão <jmacristovao@gmail.com>,\nRyan Scott <ryan.gl.scott@gmail.com>";
      author = "Simon Hengel <sol@typeful.net>,\nJoão Cristóvão <jmacristovao@gmail.com>,\nRyan Scott <ryan.gl.scott@gmail.com>";
      homepage = "";
      url = "";
      synopsis = "A compatibility layer for base";
      description = "Provides functions available in later versions of @base@ to\na wider range of compilers, without requiring you to use CPP\npragmas in your code.  See the\n<https://github.com/haskell-compat/base-compat/blob/master/base-compat/README.markdown README>\nfor what is covered. Also see the\n<https://github.com/haskell-compat/base-compat/blob/master/base-compat/CHANGES.markdown changelog>\nfor recent changes.\n\nNote that @base-compat@ does not add any orphan instances.\nThere is a separate package,\n@<http://hackage.haskell.org/package/base-orphans base-orphans>@,\nfor that.\n\nIn addition, @base-compat@ does not backport any data types\nor type classes. See\n@<https://github.com/haskell-compat/base-compat/blob/master/base-compat/README.markdown#data-types-and-type-classes this section of the README>@\nfor more info.\n\n@base-compat@ is designed to have zero dependencies. For a\nversion of @base-compat@ that depends on compatibility\nlibraries for a wider support window, see the\n@<http://hackage.haskell.org/package/base-compat-batteries base-compat-batteries>@\npackage. Most of the modules in this library have the same\nnames as in @base-compat-batteries@ to make it easier to\nswitch between the two. There also exist versions of each\nmodule with the suffix @.Repl@, which are distinct from\nanything in @base-compat-batteries@, to allow for easier\nuse in GHCi.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          ] ++ (pkgs.lib).optional (!system.isWindows && !system.isHalvm) (hsPkgs."unix" or (errorHandler.buildDepError "unix"));
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/base-compat-0.11.2.tar.gz";
      sha256 = "53a6b5145442fba5a4bad6db2bcdede17f164642b48bc39b95015422a39adbdb";
      });
    }