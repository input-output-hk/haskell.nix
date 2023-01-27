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
    }) // {
    package-description-override = "-- stringsearch.cabal auto-generated by cabal init. For additional\r\n-- options, see\r\n-- http://www.haskell.org/cabal/release/cabal-latest/doc/users-guide/authors.html#pkg-descr.\r\n-- The name of the package.\r\nName:                stringsearch\r\n\r\n-- The package version. See the Haskell package versioning policy\r\n-- (http://www.haskell.org/haskellwiki/Package_versioning_policy) for\r\n-- standards guiding when and how versions should be incremented.\r\nVersion:             0.3.6.6\r\nx-revision: 1\r\n\r\nHomepage:            https://bitbucket.org/dafis/stringsearch\r\nBug-reports:         https://bitbucket.org/dafis/stringsearch/issues\r\n\r\n-- A short (one-line) description of the package.\r\nSynopsis:            Fast searching, splitting and replacing of ByteStrings\r\n\r\n-- A longer description of the package.\r\nDescription:         This package provides several functions to quickly\r\n                     search for substrings in strict or lazy ByteStrings.\r\n                     It also provides functions for breaking or splitting\r\n                     on substrings and replacing all occurrences of a\r\n                     substring (the first in case of overlaps) with another.\r\n\r\n                     GHC before 6.10 are no longer supported, other compilers\r\n                     only if they support BangPatterns. If you need it to\r\n                     work with other compilers, send a feature request.\r\n\r\n\r\n-- The license under which the package is released.\r\nLicense:             BSD3\r\n\r\n-- The file containing the license text.\r\nLicense-file:        LICENCE\r\n\r\n-- The package author(s).\r\nAuthor:              Daniel Fischer, Chris Kuklewicz, Justin Bailey\r\n\r\n-- An email address to which users can send suggestions, bug reports,\r\n-- and patches.\r\nMaintainer:          daniel.is.fischer@googlemail.com\r\n\r\n-- A copyright notice.\r\nCopyright:           (c) 2007-2011\r\n                     Daniel Fischer, Chris Kuklewicz, Justin Bailey\r\n\r\nCategory:            Text, Search\r\n\r\nBuild-type:          Simple\r\n\r\n-- Extra files to be distributed with the package, such as examples or\r\n-- a README.\r\nExtra-source-files:  CHANGES\r\n\r\nTested-with:         GHC == 6.10.4, GHC == 6.12.3, GHC == 7.0.2,\r\n                     GHC == 7.0.4, GHC == 7.2.1\r\n\r\n-- Constraint on the version of Cabal needed to build this package.\r\nCabal-version:       >=1.6\r\n\r\nFlag base4\r\n  Description:       Choose base-4.*\r\n\r\nFlag base3\r\n  Description:       Choose base-3.* if base-4 isn't available\r\n  Default:           False\r\n\r\n\r\nLibrary\r\n  -- Modules exported by the library.\r\n  Exposed-modules:   Data.ByteString.Search\r\n                     Data.ByteString.Search.BoyerMoore\r\n                     Data.ByteString.Search.DFA\r\n                     Data.ByteString.Search.KarpRabin\r\n                     Data.ByteString.Search.KMP\r\n                     Data.ByteString.Search.KnuthMorrisPratt\r\n                     Data.ByteString.Search.Substitution\r\n                     Data.ByteString.Lazy.Search\r\n                     Data.ByteString.Lazy.Search.DFA\r\n                     Data.ByteString.Lazy.Search.KarpRabin\r\n                     Data.ByteString.Lazy.Search.KMP\r\n\r\n  -- Packages needed in order to build this package.\r\n  if flag(base4)\r\n    Build-depends:   base >= 4 && < 5, array >= 0.3 && < 0.6,\r\n                     bytestring >= 0.9 && < 1, containers >= 0.3 && < 0.7\r\n  else\r\n    if flag(base3)\r\n      Build-depends: base >= 3 && < 4, array >= 0.1 && < 0.4,\r\n                     bytestring >= 0.9 && < 1, containers >= 0.1 && < 0.4\r\n    else\r\n      Build-depends: base >= 2 && < 3\r\n\r\n  Extensions:        BangPatterns\r\n  if flag(base4)\r\n    ghc-options:     -O2 -fspec-constr-count=4 -Wall\r\n  else\r\n    ghc-options:     -O2 -Wall\r\n  ghc-prof-options:  -auto\r\n\r\n  -- Modules not exported by this package.\r\n  Other-modules:     Data.ByteString.Search.Internal.BoyerMoore\r\n                     Data.ByteString.Search.Internal.KnuthMorrisPratt\r\n                     Data.ByteString.Search.Internal.Utils\r\n                     Data.ByteString.Lazy.Search.Internal.BoyerMoore\r\n\r\n  -- Extra tools (e.g. alex, hsc2hs, ...) needed to build the source.\r\n  -- Build-tools:\r\n\r\nsource-repository head\r\n  type:     mercurial\r\n  location: https://bitbucket.org/dafis/stringsearch\r\n";
    }