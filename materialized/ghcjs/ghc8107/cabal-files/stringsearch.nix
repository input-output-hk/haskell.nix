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
    flags = { base4 = true; base3 = false; };
    package = {
      specVersion = "1.6";
      identifier = { name = "stringsearch"; version = "0.3.6.6"; };
      license = "BSD-3-Clause";
      copyright = "(c) 2007-2011\nDaniel Fischer, Chris Kuklewicz, Justin Bailey";
      maintainer = "daniel.is.fischer@googlemail.com";
      author = "Daniel Fischer, Chris Kuklewicz, Justin Bailey";
      homepage = "https://bitbucket.org/dafis/stringsearch";
      url = "";
      synopsis = "Fast searching, splitting and replacing of ByteStrings";
      description = "This package provides several functions to quickly\nsearch for substrings in strict or lazy ByteStrings.\nIt also provides functions for breaking or splitting\non substrings and replacing all occurrences of a\nsubstring (the first in case of overlaps) with another.\nGHC before 6.10 are no longer supported, other compilers\nonly if they support BangPatterns. If you need it to\nwork with other compilers, send a feature request.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = if flags.base4
          then [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."array" or (errorHandler.buildDepError "array"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            ]
          else if flags.base3
            then [
              (hsPkgs."base" or (errorHandler.buildDepError "base"))
              (hsPkgs."array" or (errorHandler.buildDepError "array"))
              (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
              (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
              ]
            else [ (hsPkgs."base" or (errorHandler.buildDepError "base")) ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/stringsearch-0.3.6.6.tar.gz";
      sha256 = "295f1971920bc52263d8275d7054ad223a7e1aefe75533f9887735c9644ffe4a";
      });
    }