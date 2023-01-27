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
    flags = { force-o2 = false; };
    package = {
      specVersion = "1.12";
      identifier = { name = "regex-tdfa"; version = "1.3.2"; };
      license = "BSD-3-Clause";
      copyright = "Copyright (c) 2007-2009, Christopher Kuklewicz";
      maintainer = "Andreas Abel";
      author = "Christopher Kuklewicz";
      homepage = "https://wiki.haskell.org/Regular_expressions";
      url = "";
      synopsis = "Pure Haskell Tagged DFA Backend for \"Text.Regex\" (regex-base)";
      description = "This package provides a pure Haskell \\\"Tagged\\\" DFA regex engine for <//hackage.haskell.org/package/regex-base regex-base>. This implementation was inspired by the algorithm (and Master's thesis) behind the regular expression library known as <https://github.com/laurikari/tre/ TRE or libtre>.\n\nPlease consult the \"Text.Regex.TDFA\" module for API documentation including a tutorial with usage examples;\nsee also <https://wiki.haskell.org/Regular_expressions> for general information about regular expression support in Haskell.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."parsec" or (errorHandler.buildDepError "parsec"))
          (hsPkgs."regex-base" or (errorHandler.buildDepError "regex-base"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          ] ++ (pkgs.lib).optionals (!(compiler.isGhc && (compiler.version).ge "8.0")) [
          (hsPkgs."fail" or (errorHandler.buildDepError "fail"))
          (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
          ];
        buildable = true;
        };
      tests = {
        "regex-tdfa-unittest" = {
          depends = [
            (hsPkgs."regex-tdfa" or (errorHandler.buildDepError "regex-tdfa"))
            (hsPkgs."array" or (errorHandler.buildDepError "array"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."regex-base" or (errorHandler.buildDepError "regex-base"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."utf8-string" or (errorHandler.buildDepError "utf8-string"))
            ] ++ (pkgs.lib).optionals (!(compiler.isGhc && (compiler.version).ge "8.0")) [
            (hsPkgs."fail" or (errorHandler.buildDepError "fail"))
            (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
            ];
          buildable = true;
          };
        "doc-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."regex-tdfa" or (errorHandler.buildDepError "regex-tdfa"))
            (hsPkgs."doctest-parallel" or (errorHandler.buildDepError "doctest-parallel"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/regex-tdfa-1.3.2.tar.gz";
      sha256 = "2660d7dc5f00718c39454c844d354dea26d605db9864f80951efad5f7284dfd1";
      });
    }) // {
    package-description-override = "cabal-version:          1.12\nname:                   regex-tdfa\nversion:                1.3.2\n\nbuild-Type:             Simple\nlicense:                BSD3\nlicense-file:           LICENSE\ncopyright:              Copyright (c) 2007-2009, Christopher Kuklewicz\nauthor:                 Christopher Kuklewicz\nmaintainer:             Andreas Abel\nhomepage:               https://wiki.haskell.org/Regular_expressions\nbug-reports:            https://github.com/haskell-hvr/regex-tdfa/issues\n\ncategory:               Text\nsynopsis:               Pure Haskell Tagged DFA Backend for \"Text.Regex\" (regex-base)\ndescription:\n  This package provides a pure Haskell \\\"Tagged\\\" DFA regex engine for <//hackage.haskell.org/package/regex-base regex-base>. This implementation was inspired by the algorithm (and Master's thesis) behind the regular expression library known as <https://github.com/laurikari/tre/ TRE or libtre>.\n  .\n  Please consult the \"Text.Regex.TDFA\" module for API documentation including a tutorial with usage examples;\n  see also <https://wiki.haskell.org/Regular_expressions> for general information about regular expression support in Haskell.\n\nextra-source-files:\n  CHANGELOG.md\n  README.md\n  test/cases/*.txt\n\ntested-with:\n  GHC == 9.4.1\n  GHC == 9.2.3\n  GHC == 9.0.2\n  GHC == 8.10.7\n  GHC == 8.8.4\n  GHC == 8.6.5\n  GHC == 8.4.4\n  GHC == 8.2.2\n  GHC == 8.0.2\n  GHC == 7.10.3\n  GHC == 7.8.4\n  GHC == 7.6.3\n  GHC == 7.4.2\n\nsource-repository head\n  type:                git\n  location:            https://github.com/haskell-hvr/regex-tdfa.git\n\nsource-repository this\n  type:                git\n  location:            https://github.com/haskell-hvr/regex-tdfa.git\n  tag:                 v1.3.2\n\nflag force-O2\n  default: False\n  manual: True\n  description:\n    Force building @regex-tdfa@ with \\\"@ghc-options: -O2@\\\".\n    .\n    __NOTE__: This flag is mostly provided for legacy use-cases. Nowadays you can conveniently control optimization levels on a per-package granularity via @cabal.project@ files; see <https://cabal.readthedocs.io/en/latest/nix-local-build.html#configuring-builds-with-cabal-project cabal's user-guide> for more details.\n\nlibrary\n  hs-source-dirs:       lib\n\n  exposed-modules:      Data.IntMap.CharMap2\n                        Data.IntMap.EnumMap2\n                        Data.IntSet.EnumSet2\n                        Text.Regex.TDFA\n                        Text.Regex.TDFA.ByteString\n                        Text.Regex.TDFA.ByteString.Lazy\n                        Text.Regex.TDFA.Common\n                        Text.Regex.TDFA.CorePattern\n                        Text.Regex.TDFA.IntArrTrieSet\n                        Text.Regex.TDFA.NewDFA.Engine\n                        Text.Regex.TDFA.NewDFA.Engine_FA\n                        Text.Regex.TDFA.NewDFA.Engine_NC\n                        Text.Regex.TDFA.NewDFA.Engine_NC_FA\n                        Text.Regex.TDFA.NewDFA.Tester\n                        Text.Regex.TDFA.NewDFA.Uncons\n                        Text.Regex.TDFA.NewDFA.MakeTest\n                        Text.Regex.TDFA.Pattern\n                        Text.Regex.TDFA.ReadRegex\n                        Text.Regex.TDFA.Sequence\n                        Text.Regex.TDFA.String\n                        Text.Regex.TDFA.TDFA\n                        Text.Regex.TDFA.TNFA\n                        Text.Regex.TDFA.Text\n                        Text.Regex.TDFA.Text.Lazy\n\n  other-modules:        Paths_regex_tdfa\n\n  -- Support Semigroup instances uniformly\n  --\n  -- See also\n  --  https://prime.haskell.org/wiki/Libraries/Proposals/SemigroupMonoid#RecommendedVariant\n  --\n  -- NB: This is the same logic `parsec.cabal` uses, so this doesn't\n  -- add any new dependency that isn't already incurred by\n  -- `regex-tdfa`'s transitive deps\n  if !impl(ghc >= 8.0)\n    build-depends:      fail               == 4.9.*\n                      , semigroups         == 0.18.* || == 0.19.*\n  build-depends:        array              >= 0.4    && < 0.6\n                      , base               >= 4.5    && < 5\n                      , bytestring         >= 0.9.2  && < 0.12\n                      , containers         >= 0.4.2  && < 0.7\n                      , mtl                >= 2.1.3  && < 2.4\n                      , parsec             == 3.1.*\n                      , regex-base         == 0.94.*\n                      , text               >= 1.2.3  && < 2.1\n\n  default-language:     Haskell2010\n  default-extensions:   BangPatterns\n                        ExistentialQuantification\n                        FlexibleContexts\n                        FlexibleInstances\n                        ForeignFunctionInterface\n                        FunctionalDependencies\n                        MagicHash\n                        MultiParamTypeClasses\n                        NondecreasingIndentation\n                        RecursiveDo\n                        ScopedTypeVariables\n                        TypeOperators\n                        TypeSynonymInstances\n                        UnboxedTuples\n                        UnliftedFFITypes\n  other-extensions:     CPP\n\n  ghc-options:          -Wall -funbox-strict-fields -fspec-constr-count=10 -fno-warn-orphans\n\n  if impl(ghc >= 8.0)\n    ghc-options:        -Wcompat\n\n  if flag(force-O2)\n    ghc-options:        -O2\n\n\ntest-suite regex-tdfa-unittest\n  type:                 exitcode-stdio-1.0\n\n  hs-source-dirs:       test\n  main-is:              Main.hs\n\n  -- intra-package dependency\n  build-depends:        regex-tdfa\n\n  -- dependencies whose version constraints are inherited via intra-package 'regex-tdfa' dependency\n  if !impl(ghc >= 8.0)\n    build-depends:      fail\n                      , semigroups\n  build-depends:        array\n                      , base\n                      , bytestring\n                      , containers\n                      , filepath\n                      , mtl\n                      , regex-base\n                      , text\n\n  -- component-specific dependencies not inherited via 'regex-tdfa'\n                      , directory          >= 1.1.0  && < 1.4\n                      , filepath           >= 1.3.0  && < 1.5\n                      , utf8-string        >= 1.0.1  && < 1.1\n\n  default-language:     Haskell2010\n  default-extensions:   FlexibleInstances\n                        FlexibleContexts\n                        Rank2Types\n  other-extensions:     GeneralizedNewtypeDeriving\n\n  ghc-options:          -Wall -funbox-strict-fields\n\n  if impl(ghc >= 8.0)\n    ghc-options:        -Wcompat\n\n  if flag(force-O2)\n    ghc-options:        -O2\n\ntest-suite doc-test\n  type:           exitcode-stdio-1.0\n  hs-source-dirs: test\n  main-is:        DocTestMain.hs\n\n  build-depends:\n      base\n    , regex-tdfa\n    , doctest-parallel >= 0.2.2\n        -- doctest-parallel-0.2.2 is the first to filter out autogen-modules\n\n  default-language:     Haskell2010\n";
    }