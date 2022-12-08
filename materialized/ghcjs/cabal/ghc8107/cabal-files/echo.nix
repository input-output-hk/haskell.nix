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
      identifier = { name = "echo"; version = "0.1.4"; };
      license = "BSD-3-Clause";
      copyright = "(C) 2016-2017 Ryan Scott";
      maintainer = "Ryan Scott <ryan.gl.scott@gmail.com>";
      author = "Ryan Scott";
      homepage = "https://github.com/RyanGlScott/echo";
      url = "";
      synopsis = "A cross-platform, cross-console way to handle echoing terminal input";
      description = "The @base@ library exposes the @hGetEcho@ and @hSetEcho@ functions\nfor querying and setting echo status, but unfortunately, neither\nfunction works with MinTTY consoles on Windows. This is a serious\nissue, since @hGetEcho@ and @hSetEcho@ are often used to disable\ninput echoing when a program prompts for a password, so many\nprograms will reveal your password as you type it on MinTTY!\n\nThis library provides an alternative interface which works\nwith both MinTTY and other consoles. An example is included\nwhich demonstrates how one might prompt for a password using\nthis library. To build it, make sure to configure with the\n@-fexample@ flag.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."process" or (errorHandler.buildDepError "process"))
          ] ++ (pkgs.lib).optionals (system.isWindows) [
          (hsPkgs."mintty" or (errorHandler.buildDepError "mintty"))
          (hsPkgs."Win32" or (errorHandler.buildDepError "Win32"))
          ];
        buildable = true;
        };
      exes = {
        "password" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."echo" or (errorHandler.buildDepError "echo"))
            ];
          buildable = if !flags.example then false else true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/echo-0.1.4.tar.gz";
      sha256 = "c9fe1bf2904825a65b667251ec644f197b71dc5c209d2d254be5de3d496b0e43";
      });
    }