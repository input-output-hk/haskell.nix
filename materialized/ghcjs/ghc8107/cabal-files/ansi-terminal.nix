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
    flags = { example = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "ansi-terminal"; version = "0.11"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Mike Pilgrem <public@pilgrem.com>, Roman Cheplyaka <roma@ro-che.info>";
      author = "Max Bolingbroke";
      homepage = "https://github.com/feuerbach/ansi-terminal";
      url = "";
      synopsis = "Simple ANSI terminal support, with Windows compatibility";
      description = "ANSI terminal support for Haskell: allows cursor movement,\nscreen clearing, color output, showing or hiding the\ncursor, and changing the title. Works on UNIX and Windows.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."colour" or (errorHandler.buildDepError "colour"))
          ] ++ (pkgs.lib).optionals (system.isWindows) [
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."mintty" or (errorHandler.buildDepError "mintty"))
          (hsPkgs."Win32" or (errorHandler.buildDepError "Win32"))
          ];
        buildable = true;
        };
      exes = {
        "ansi-terminal-example" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."ansi-terminal" or (errorHandler.buildDepError "ansi-terminal"))
            (hsPkgs."colour" or (errorHandler.buildDepError "colour"))
            ];
          buildable = if !flags.example then false else true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/ansi-terminal-0.11.tar.gz";
      sha256 = "c6611b9e51add41db3f79eac30066c06b33a6ca2a09e586b4b361d7f98303793";
      });
    }