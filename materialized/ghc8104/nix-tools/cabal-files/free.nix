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
      specVersion = "1.18";
      identifier = { name = "free"; version = "5.1.9"; };
      license = "BSD-3-Clause";
      copyright = "Copyright (C) 2008-2015 Edward A. Kmett";
      maintainer = "Edward A. Kmett <ekmett@gmail.com>";
      author = "Edward A. Kmett";
      homepage = "http://github.com/ekmett/free/";
      url = "";
      synopsis = "Monads for free";
      description = "Free monads are useful for many tree-like structures and domain specific languages.\n\nIf @f@ is a 'Functor' then the free 'Monad' on @f@ is the type\nof trees whose nodes are labeled with the constructors of @f@. The word\n\\\"free\\\" is used in the sense of \\\"unrestricted\\\" rather than \\\"zero-cost\\\":\n@Free f@ makes no constraining assumptions beyond those given by @f@ and the\ndefinition of 'Monad'. As used here it is a standard term from the\nmathematical theory of adjoint functors.\n\nCofree comonads are dual to free monads. They provide convenient ways to talk\nabout branching streams and rose-trees, and can be used to annotate syntax\ntrees. The cofree comonad can be seen as a stream parameterized by a 'Functor'\nthat controls its branching factor.\n\nMore information on free monads, including examples, can be found in the\nfollowing blog posts:\n<http://comonad.com/reader/2008/monads-for-free/>\n<http://comonad.com/reader/2011/free-monads-for-less/>";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = ((((([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."comonad" or (errorHandler.buildDepError "comonad"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."distributive" or (errorHandler.buildDepError "distributive"))
          (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
          (hsPkgs."indexed-traversable" or (errorHandler.buildDepError "indexed-traversable"))
          (hsPkgs."semigroupoids" or (errorHandler.buildDepError "semigroupoids"))
          (hsPkgs."th-abstraction" or (errorHandler.buildDepError "th-abstraction"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."transformers-base" or (errorHandler.buildDepError "transformers-base"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          ] ++ [ (hsPkgs."mtl" or (errorHandler.buildDepError "mtl")) ]) ++ [
          (hsPkgs."profunctors" or (errorHandler.buildDepError "profunctors"))
          ]) ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8.2")) (hsPkgs."bifunctors" or (errorHandler.buildDepError "bifunctors"))) ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8.0")) (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))) ++ (if compiler.isGhc && (compiler.version).ge "7.10"
          then [
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            ]
          else [
            (hsPkgs."transformers-compat" or (errorHandler.buildDepError "transformers-compat"))
            ])) ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8.0")) (hsPkgs."fail" or (errorHandler.buildDepError "fail"));
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/free-5.1.9.tar.gz";
      sha256 = "2e751309408550ebccc2708170ec8473eac1e35b4bc1016bee0776ac938e9fee";
      });
    }) // {
    package-description-override = "name:          free\r\ncategory:      Control, Monads\r\nversion:       5.1.9\r\nx-revision: 1\r\nlicense:       BSD3\r\ncabal-version: 1.18\r\nlicense-file:  LICENSE\r\nauthor:        Edward A. Kmett\r\nmaintainer:    Edward A. Kmett <ekmett@gmail.com>\r\nstability:     provisional\r\nhomepage:      http://github.com/ekmett/free/\r\nbug-reports:   http://github.com/ekmett/free/issues\r\ncopyright:     Copyright (C) 2008-2015 Edward A. Kmett\r\ntested-with:   GHC == 7.4.2\r\n             , GHC == 7.6.3\r\n             , GHC == 7.8.4\r\n             , GHC == 7.10.3\r\n             , GHC == 8.0.2\r\n             , GHC == 8.2.2\r\n             , GHC == 8.4.4\r\n             , GHC == 8.6.5\r\n             , GHC == 8.8.4\r\n             , GHC == 8.10.7\r\n             , GHC == 9.0.2\r\n             , GHC == 9.2.2\r\nsynopsis:      Monads for free\r\ndescription:\r\n  Free monads are useful for many tree-like structures and domain specific languages.\r\n  .\r\n  If @f@ is a 'Functor' then the free 'Monad' on @f@ is the type\r\n  of trees whose nodes are labeled with the constructors of @f@. The word\r\n  \\\"free\\\" is used in the sense of \\\"unrestricted\\\" rather than \\\"zero-cost\\\":\r\n  @Free f@ makes no constraining assumptions beyond those given by @f@ and the\r\n  definition of 'Monad'. As used here it is a standard term from the\r\n  mathematical theory of adjoint functors.\r\n  .\r\n  Cofree comonads are dual to free monads. They provide convenient ways to talk\r\n  about branching streams and rose-trees, and can be used to annotate syntax\r\n  trees. The cofree comonad can be seen as a stream parameterized by a 'Functor'\r\n  that controls its branching factor.\r\n  .\r\n  More information on free monads, including examples, can be found in the\r\n  following blog posts:\r\n  <http://comonad.com/reader/2008/monads-for-free/>\r\n  <http://comonad.com/reader/2011/free-monads-for-less/>\r\n\r\nbuild-type:    Simple\r\nextra-source-files:\r\n  .ghci\r\n  .gitignore\r\n  .hlint.yaml\r\n  .vim.custom\r\n  README.markdown\r\n  CHANGELOG.markdown\r\n  doc/proof/Control/Comonad/Cofree/*.md\r\n  doc/proof/Control/Comonad/Trans/Cofree/*.md\r\n  examples/free-examples.cabal\r\n  examples/LICENSE\r\n  examples/*.hs\r\n  examples/*.lhs\r\n  include/free-common.h\r\nextra-doc-files:\r\n  examples/*.hs\r\n  examples/*.lhs\r\n\r\nsource-repository head\r\n  type: git\r\n  location: git://github.com/ekmett/free.git\r\n\r\nlibrary\r\n  hs-source-dirs: src\r\n  include-dirs: include\r\n  includes: free-common.h\r\n\r\n  default-language:   Haskell2010\r\n  default-extensions: CPP\r\n  other-extensions:\r\n    MultiParamTypeClasses\r\n    FunctionalDependencies\r\n    FlexibleInstances\r\n    UndecidableInstances\r\n    Rank2Types\r\n    GADTs\r\n\r\n  build-depends:\r\n    base                 >= 4.5     && < 5,\r\n    comonad              >= 5.0.8   && < 6,\r\n    containers           >= 0.3     && < 0.7,\r\n    distributive         >= 0.5.2   && < 1,\r\n    exceptions           >= 0.10.4  && < 0.11,\r\n    indexed-traversable  >= 0.1.1   && < 0.2,\r\n    semigroupoids        >= 5.3.5   && < 6,\r\n    th-abstraction       >= 0.4.2.0 && < 0.5,\r\n    transformers         >= 0.3     && < 0.7,\r\n    transformers-base    >= 0.4.5.2 && < 0.5,\r\n    template-haskell     >= 2.7.0.0 && < 2.20\r\n\r\n  -- GHC-7.8 bundles transformers-0.3,\r\n  -- mtl-2.2.* requires transformers >=0.4\r\n  if impl(ghc >=7.10)\r\n    build-depends:\r\n      mtl               >= 2.2.2 && < 2.4\r\n  else\r\n    build-depends:\r\n      mtl               >= 2.1.3.1 && < 2.4\r\n\r\n  -- recent profunctors dropped support for GHCs older than 7.8\r\n  if impl(ghc >=7.8)\r\n    build-depends:\r\n      profunctors >= 5.6.1 && < 6\r\n  else\r\n    build-depends:\r\n      profunctors >= 5.2.2 && < 5.3\r\n\r\n  if !impl(ghc >= 8.2)\r\n    build-depends: bifunctors >= 5.5.9 && < 6\r\n\r\n  if !impl(ghc >= 8.0)\r\n    build-depends: semigroups >= 0.18.5 && < 1\r\n\r\n  -- Ensure Data.Functor.Classes is always available\r\n  if impl(ghc >= 7.10)\r\n    build-depends: transformers >= 0.4.2.0\r\n  else\r\n    build-depends: transformers-compat >= 0.5.1.0 && <0.8\r\n\r\n  exposed-modules:\r\n    Control.Applicative.Free\r\n    Control.Applicative.Free.Fast\r\n    Control.Applicative.Free.Final\r\n    Control.Applicative.Trans.Free\r\n    Control.Alternative.Free\r\n    Control.Alternative.Free.Final\r\n    Control.Comonad.Cofree\r\n    Control.Comonad.Cofree.Class\r\n    Control.Comonad.Trans.Cofree\r\n    Control.Comonad.Trans.Coiter\r\n    Control.Monad.Free\r\n    Control.Monad.Free.Ap\r\n    Control.Monad.Free.Church\r\n    Control.Monad.Free.Class\r\n    Control.Monad.Free.TH\r\n    Control.Monad.Trans.Free\r\n    Control.Monad.Trans.Free.Ap\r\n    Control.Monad.Trans.Free.Church\r\n    Control.Monad.Trans.Iter\r\n\r\n  other-modules:\r\n    Data.Functor.Classes.Compat\r\n\r\n  ghc-options: -Wall\r\n\r\n  -- See https://ghc.haskell.org/trac/ghc/wiki/Migration/8.0#base-4.9.0.0\r\n  if impl(ghc >= 8.0)\r\n    ghc-options: -Wcompat -Wnoncanonical-monad-instances\r\n\r\n    if !impl(ghc >= 8.8)\r\n      ghc-options: -Wnoncanonical-monadfail-instances\r\n  else\r\n    build-depends: fail == 4.9.*\r\n\r\n  if impl(ghc >= 9.0)\r\n    -- these flags may abort compilation with GHC-8.10\r\n    -- https://gitlab.haskell.org/ghc/ghc/-/merge_requests/3295\r\n    ghc-options: -Winferred-safe-imports -Wmissing-safe-haskell-mode\r\n\r\n  x-docspec-extra-packages: tagged\r\n";
    }