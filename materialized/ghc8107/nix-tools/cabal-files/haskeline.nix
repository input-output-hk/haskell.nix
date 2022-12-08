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
    flags = { terminfo = true; examples = true; };
    package = {
      specVersion = "1.10";
      identifier = { name = "haskeline"; version = "0.8.2"; };
      license = "BSD-3-Clause";
      copyright = "(c) Judah Jacobson";
      maintainer = "Judah Jacobson <judah.jacobson@gmail.com>";
      author = "Judah Jacobson";
      homepage = "https://github.com/judah/haskeline";
      url = "";
      synopsis = "A command-line interface for user input, written in Haskell.";
      description = "Haskeline provides a user interface for line input in command-line\nprograms.  This library is similar in purpose to readline, but since\nit is written in Haskell it is (hopefully) more easily used in other\nHaskell programs.\n\nHaskeline runs both on POSIX-compatible systems and on Windows.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."process" or (errorHandler.buildDepError "process"))
          (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
          (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
          ] ++ (if system.isWindows
          then [ (hsPkgs."Win32" or (errorHandler.buildDepError "Win32")) ]
          else [
            (hsPkgs."unix" or (errorHandler.buildDepError "unix"))
            ] ++ (pkgs.lib).optional (flags.terminfo) (hsPkgs."terminfo" or (errorHandler.buildDepError "terminfo")));
        buildable = true;
        };
      exes = {
        "haskeline-examples-Test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."haskeline" or (errorHandler.buildDepError "haskeline"))
            ];
          buildable = if !flags.examples then false else true;
          };
        };
      tests = {
        "haskeline-tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."process" or (errorHandler.buildDepError "process"))
            (hsPkgs."unix" or (errorHandler.buildDepError "unix"))
            ];
          build-tools = [
            (hsPkgs.buildPackages.haskeline.components.exes.haskeline-examples-Test or (pkgs.buildPackages.haskeline-examples-Test or (errorHandler.buildToolDepError "haskeline:haskeline-examples-Test")))
            ];
          buildable = (if system.isWindows
            then false
            else true) && (if !flags.examples then false else true);
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/haskeline-0.8.2.tar.gz";
      sha256 = "ec9d155c5aff7489c2553e57130ed66f89857188c7a06108900c8d1066fc27df";
      });
    }