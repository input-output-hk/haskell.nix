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
      specVersion = "3.0";
      identifier = { name = "base"; version = "4.19.2.0"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Core Libraries Committee <core-libraries-committee@haskell.org>";
      author = "";
      homepage = "";
      url = "";
      synopsis = "Core data structures and operations";
      description = "Haskell's base library provides, among other things, core types (e.g. [Bool](\"Data.Bool\") and [Int](\"Data.Int\")),\ndata structures (e.g. [List](\"Data.List\"), [Tuple](\"Data.Tuple\") and [Maybe](\"Data.Maybe\")),\nthe [Exception](\"Control.Exception\") mechanism, and the [IO](\"System.IO\") & [Concurrency](\"Control.Concurrent\") operations.\nThe \"Prelude\" module, which is imported by default, exposes a curated set of types and functions from other modules.\n\nOther data structures like [Map](https://hackage.haskell.org/package/containers/docs/Data-Map.html),\n[Set](https://hackage.haskell.org/package/containers/docs/Data-Set.html) are available in the [containers](https://hackage.haskell.org/package/containers) library.\nTo work with textual data, use the [text](https://hackage.haskell.org/package/text/docs/Data-Text.html) library.";
      buildType = "Configure";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."rts" or (errorHandler.buildDepError "rts"))
          (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
          (hsPkgs."ghc-bignum" or (errorHandler.buildDepError "ghc-bignum"))
        ];
        libs = pkgs.lib.optionals (system.isWindows) [
          (pkgs."wsock32" or (errorHandler.sysDepError "wsock32"))
          (pkgs."user32" or (errorHandler.sysDepError "user32"))
          (pkgs."shell32" or (errorHandler.sysDepError "shell32"))
          (pkgs."mingw32" or (errorHandler.sysDepError "mingw32"))
          (pkgs."kernel32" or (errorHandler.sysDepError "kernel32"))
          (pkgs."advapi32" or (errorHandler.sysDepError "advapi32"))
          (pkgs."mingwex" or (errorHandler.sysDepError "mingwex"))
          (pkgs."ws2_32" or (errorHandler.sysDepError "ws2_32"))
          (pkgs."shlwapi" or (errorHandler.sysDepError "shlwapi"))
          (pkgs."ole32" or (errorHandler.sysDepError "ole32"))
          (pkgs."rpcrt4" or (errorHandler.sysDepError "rpcrt4"))
          (pkgs."ntdll" or (errorHandler.sysDepError "ntdll"))
        ];
        buildable = true;
      };
    };
  } // rec { src = pkgs.lib.mkDefault ./.; }
