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
      identifier = { name = "profunctors"; version = "5.6.2"; };
      license = "BSD-3-Clause";
      copyright = "Copyright (C) 2011-2015 Edward A. Kmett";
      maintainer = "Edward A. Kmett <ekmett@gmail.com>";
      author = "Edward A. Kmett";
      homepage = "http://github.com/ekmett/profunctors/";
      url = "";
      synopsis = "Profunctors";
      description = "Profunctors.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."base-orphans" or (errorHandler.buildDepError "base-orphans"))
          (hsPkgs."bifunctors" or (errorHandler.buildDepError "bifunctors"))
          (hsPkgs."comonad" or (errorHandler.buildDepError "comonad"))
          (hsPkgs."contravariant" or (errorHandler.buildDepError "contravariant"))
          (hsPkgs."distributive" or (errorHandler.buildDepError "distributive"))
          (hsPkgs."tagged" or (errorHandler.buildDepError "tagged"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          ] ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8.0")) (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"));
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/profunctors-5.6.2.tar.gz";
      sha256 = "65955d7b50525a4a3bccdab1d982d2ae342897fd38140d5a94b5ef3800d8c92a";
      });
    }) // {
    package-description-override = "name:          profunctors\r\ncategory:      Control, Categories\r\nversion:       5.6.2\r\nx-revision: 2\r\nlicense:       BSD3\r\ncabal-version: >= 1.10\r\nlicense-file:  LICENSE\r\nauthor:        Edward A. Kmett\r\nmaintainer:    Edward A. Kmett <ekmett@gmail.com>\r\nstability:     experimental\r\nhomepage:      http://github.com/ekmett/profunctors/\r\nbug-reports:   http://github.com/ekmett/profunctors/issues\r\ncopyright:     Copyright (C) 2011-2015 Edward A. Kmett\r\nsynopsis:      Profunctors\r\ndescription:   Profunctors.\r\ntested-with:   GHC == 7.8.4\r\n             , GHC == 7.10.3\r\n             , GHC == 8.0.2\r\n             , GHC == 8.2.2\r\n             , GHC == 8.4.4\r\n             , GHC == 8.6.5\r\n             , GHC == 8.8.3\r\n             , GHC == 8.10.1\r\nbuild-type:    Simple\r\nextra-source-files:\r\n  .ghci\r\n  .gitignore\r\n  .hlint.yaml\r\n  .vim.custom\r\n  README.markdown\r\n  CHANGELOG.markdown\r\n\r\nsource-repository head\r\n  type: git\r\n  location: git://github.com/ekmett/profunctors.git\r\n\r\nlibrary\r\n  build-depends:\r\n    base                >= 4.7     && < 5,\r\n    base-orphans        >= 0.8.4   && < 0.9,\r\n    bifunctors          >= 5.5.9   && < 6,\r\n    comonad             >= 5.0.8   && < 6,\r\n    contravariant       >= 1.5.3   && < 2,\r\n    distributive        >= 0.5.2   && < 1,\r\n    tagged              >= 0.8.6.1 && < 1,\r\n    transformers        >= 0.3     && < 0.7\r\n\r\n  if !impl(ghc >= 8.0)\r\n    build-depends: semigroups >= 0.18.5 && < 0.21\r\n\r\n  exposed-modules:\r\n    Data.Profunctor\r\n    Data.Profunctor.Adjunction\r\n    Data.Profunctor.Cayley\r\n    Data.Profunctor.Choice\r\n    Data.Profunctor.Closed\r\n    Data.Profunctor.Composition\r\n    Data.Profunctor.Mapping\r\n    Data.Profunctor.Monad\r\n    Data.Profunctor.Ran\r\n    Data.Profunctor.Rep\r\n    Data.Profunctor.Sieve\r\n    Data.Profunctor.Strong\r\n    Data.Profunctor.Traversing\r\n    Data.Profunctor.Types\r\n    Data.Profunctor.Unsafe\r\n    Data.Profunctor.Yoneda\r\n\r\n  ghc-options:     -Wall -O2\r\n\r\n  if impl(ghc>=8.0)\r\n    ghc-options: -Wno-trustworthy-safe\r\n\r\n  if impl(ghc >= 8.6)\r\n    ghc-options: -Wno-star-is-type\r\n\r\n  if impl(ghc >= 9.0)\r\n    -- these flags may abort compilation with GHC-8.10\r\n    -- https://gitlab.haskell.org/ghc/ghc/-/merge_requests/3295\r\n    ghc-options: -Winferred-safe-imports -Wmissing-safe-haskell-mode\r\n\r\n  hs-source-dirs:  src\r\n\r\n  default-language: Haskell2010\r\n  other-extensions:\r\n    CPP\r\n    GADTs\r\n    FlexibleContexts\r\n    FlexibleInstances\r\n    InstanceSigs\r\n    UndecidableInstances\r\n    TypeFamilies\r\n";
    }