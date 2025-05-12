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
      identifier = { name = "base"; version = "4.20.1.0"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Core Libraries Committee <core-libraries-committee@haskell.org>";
      author = "";
      homepage = "";
      url = "";
      synopsis = "Core data structures and operations";
      description = "Haskell's base library provides, among other things, core types (e.g. [Bool](\"Data.Bool\") and [Int](\"Data.Int\")),\ndata structures (e.g. [List](\"Data.List\"), [Tuple](\"Data.Tuple\") and [Maybe](\"Data.Maybe\")),\nthe [Exception](\"Control.Exception\") mechanism, and the [IO](\"System.IO\") & [Concurrency](\"Control.Concurrent\") operations.\nThe \"Prelude\" module, which is imported by default, exposes a curated set of types and functions from other modules.\n\nOther data structures like [Map](https://hackage.haskell.org/package/containers/docs/Data-Map.html),\n[Set](https://hackage.haskell.org/package/containers/docs/Data-Set.html) are available in the [containers](https://hackage.haskell.org/package/containers) library.\nTo work with textual data, use the [text](https://hackage.haskell.org/package/text/docs/Data-Text.html) library.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."ghc-internal" or (errorHandler.buildDepError "ghc-internal"))
          (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
        ];
        buildable = true;
      };
    };
  } // rec { src = pkgs.lib.mkDefault ./.; }
