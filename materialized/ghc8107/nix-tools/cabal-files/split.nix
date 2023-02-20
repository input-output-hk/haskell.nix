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
      specVersion = "1.10";
      identifier = { name = "split"; version = "0.2.3.5"; };
      license = "BSD-3-Clause";
      copyright = "(c) Brent Yorgey, Louis Wasserman 2008-2012";
      maintainer = "byorgey@gmail.com";
      author = "Brent Yorgey";
      homepage = "";
      url = "";
      synopsis = "Combinator library for splitting lists.";
      description = "A collection of various methods for splitting\nlists into parts, akin to the \\\"split\\\" function\nfound in several mainstream languages. Here is\nits tale:\n\nOnce upon a time the standard \"Data.List\" module\nheld no function for splitting a list into parts\naccording to a delimiter.  Many a brave\nlambda-knight strove to add such a function, but\ntheir striving was in vain, for Lo, the Supreme\nCouncil fell to bickering amongst themselves what\nwas to be the essential nature of the One True\nFunction which could cleave a list in twain (or\nthrain, or any required number of parts).\n\nAnd thus came to pass the split package,\ncomprising divers functions for splitting a list\nasunder, each according to its nature.  And the\nSupreme Council had no longer any grounds for\nargument, for the favored method of each was\ncontained therein.\n\nTo get started, see the \"Data.List.Split\" module.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [ (hsPkgs."base" or (errorHandler.buildDepError "base")) ];
        buildable = true;
        };
      tests = {
        "split-tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."split" or (errorHandler.buildDepError "split"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/split-0.2.3.5.tar.gz";
      sha256 = "bf8aa8d610354a2b576946a6c838251ec5988c8374100638e6b2604513b93159";
      });
    }) // {
    package-description-override = "Name:                split\nVersion:             0.2.3.5\nStability:           stable\n\nDescription:         A collection of various methods for splitting\n                     lists into parts, akin to the \\\"split\\\" function\n                     found in several mainstream languages. Here is\n                     its tale:\n                     .\n                     Once upon a time the standard \"Data.List\" module\n                     held no function for splitting a list into parts\n                     according to a delimiter.  Many a brave\n                     lambda-knight strove to add such a function, but\n                     their striving was in vain, for Lo, the Supreme\n                     Council fell to bickering amongst themselves what\n                     was to be the essential nature of the One True\n                     Function which could cleave a list in twain (or\n                     thrain, or any required number of parts).\n                     .\n                     And thus came to pass the split package,\n                     comprising divers functions for splitting a list\n                     asunder, each according to its nature.  And the\n                     Supreme Council had no longer any grounds for\n                     argument, for the favored method of each was\n                     contained therein.\n                     .\n                     To get started, see the \"Data.List.Split\" module.\nSynopsis:            Combinator library for splitting lists.\nLicense:             BSD3\nLicense-file:        LICENSE\nCopyright:           (c) Brent Yorgey, Louis Wasserman 2008-2012\nExtra-source-files:  README, test/Properties.hs, CHANGES\nAuthor:              Brent Yorgey\nMaintainer:          byorgey@gmail.com\nCategory:            List\nBuild-type:          Simple\nCabal-Version:       >= 1.10\nTested-with:\n  GHC == 7.0.4\n  GHC == 7.2.2\n  GHC == 7.4.2\n  GHC == 7.6.3\n  GHC == 7.8.4\n  GHC == 7.10.3\n  GHC == 8.0.2\n  GHC == 8.2.2\n  GHC == 8.4.4\n  GHC == 8.6.5\n  GHC == 8.8.4\n  GHC == 8.10.7\n  GHC == 9.0.1\n  GHC == 9.2.4\n  GHC == 9.4.1\n\nBug-reports:         https://github.com/byorgey/split/issues\n\nTest-suite split-tests\n  type:              exitcode-stdio-1.0\n  main-is:           Properties.hs\n  build-depends:     base, QuickCheck >= 2.4, split\n  default-language:  Haskell2010\n  Hs-source-dirs:    test\n\nSource-repository head\n  type:              git\n  location:          http://github.com/byorgey/split.git\n\nLibrary\n  ghc-options:       -Wall\n  build-depends:     base < 5\n  exposed-modules:   Data.List.Split, Data.List.Split.Internals\n  default-language:  Haskell2010\n  Hs-source-dirs:    src\n";
    }