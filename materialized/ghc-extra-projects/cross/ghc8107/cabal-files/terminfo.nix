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
      identifier = { name = "terminfo"; version = "0.4.1.5"; };
      license = "BSD-3-Clause";
      copyright = "(c) Judah Jacobson";
      maintainer = "Judah Jacobson <judah.jacobson@gmail.com>";
      author = "Judah Jacobson";
      homepage = "https://github.com/judah/terminfo";
      url = "";
      synopsis = "Haskell bindings to the terminfo library.";
      description = "This library provides an interface to the terminfo database (via bindings to the\ncurses library).  <http://en.wikipedia.org/wiki/Terminfo Terminfo> allows POSIX\nsystems to interact with a variety of terminals using a standard set of capabilities.";
      buildType = "Configure";
      };
    components = {
      "library" = {
        depends = [ (hsPkgs."base" or (errorHandler.buildDepError "base")) ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/terminfo-0.4.1.5.tar.gz";
      sha256 = "b160211246cfed10b920c859569e8e0f1952013b8d2d4527fd554342ed2c1d68";
      });
    }