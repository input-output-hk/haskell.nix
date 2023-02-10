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
    flags = {
      containers = true;
      distributive = true;
      indexed-traversable = true;
      };
    package = {
      specVersion = "1.10";
      identifier = { name = "comonad"; version = "5.0.8"; };
      license = "BSD-3-Clause";
      copyright = "Copyright (C) 2008-2014 Edward A. Kmett,\nCopyright (C) 2004-2008 Dave Menendez";
      maintainer = "Edward A. Kmett <ekmett@gmail.com>";
      author = "Edward A. Kmett";
      homepage = "http://github.com/ekmett/comonad/";
      url = "";
      synopsis = "Comonads";
      description = "Comonads.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = ((([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."tagged" or (errorHandler.buildDepError "tagged"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."transformers-compat" or (errorHandler.buildDepError "transformers-compat"))
          ] ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8.0")) (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))) ++ (pkgs.lib).optional (flags.containers) (hsPkgs."containers" or (errorHandler.buildDepError "containers"))) ++ (pkgs.lib).optional (flags.distributive) (hsPkgs."distributive" or (errorHandler.buildDepError "distributive"))) ++ (pkgs.lib).optional (flags.indexed-traversable) (hsPkgs."indexed-traversable" or (errorHandler.buildDepError "indexed-traversable"));
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/comonad-5.0.8.tar.gz";
      sha256 = "ef6cdf2cc292cc43ee6aa96c581b235fdea8ab44a0bffb24dc79ae2b2ef33d13";
      });
    }) // {
    package-description-override = "name:          comonad\r\ncategory:      Control, Comonads\r\nversion:       5.0.8\r\nx-revision: 1\r\nlicense:       BSD3\r\ncabal-version: >= 1.10\r\nlicense-file:  LICENSE\r\nauthor:        Edward A. Kmett\r\nmaintainer:    Edward A. Kmett <ekmett@gmail.com>\r\nstability:     provisional\r\nhomepage:      http://github.com/ekmett/comonad/\r\nbug-reports:   http://github.com/ekmett/comonad/issues\r\ncopyright:     Copyright (C) 2008-2014 Edward A. Kmett,\r\n               Copyright (C) 2004-2008 Dave Menendez\r\nsynopsis:      Comonads\r\ndescription:   Comonads.\r\nbuild-type:    Simple\r\ntested-with:   GHC == 7.0.4\r\n             , GHC == 7.2.2\r\n             , GHC == 7.4.2\r\n             , GHC == 7.6.3\r\n             , GHC == 7.8.4\r\n             , GHC == 7.10.3\r\n             , GHC == 8.0.2\r\n             , GHC == 8.2.2\r\n             , GHC == 8.4.4\r\n             , GHC == 8.6.5\r\n             , GHC == 8.8.3\r\n             , GHC == 8.10.1\r\nextra-source-files:\r\n  .gitignore\r\n  .hlint.yaml\r\n  .vim.custom\r\n  coq/Store.v\r\n  README.markdown\r\n  CHANGELOG.markdown\r\n  examples/History.hs\r\n\r\nflag containers\r\n  description:\r\n    You can disable the use of the `containers` package using `-f-containers`.\r\n    .\r\n    Disabing this is an unsupported configuration, but it may be useful for accelerating builds in sandboxes for expert users.\r\n  default: True\r\n  manual: True\r\n\r\nflag distributive\r\n  description:\r\n    You can disable the use of the `distributive` package using `-f-distributive`.\r\n    .\r\n    Disabling this is an unsupported configuration, but it may be useful for accelerating builds in sandboxes for expert users.\r\n    .\r\n    If disabled we will not supply instances of `Distributive`\r\n    .\r\n  default: True\r\n  manual: True\r\n\r\nflag indexed-traversable\r\n  description:\r\n    You can disable the use of the `indexed-traversable` package using `-f-indexed-traversable`.\r\n    .\r\n    Disabling this is an unsupported configuration, but it may be useful for accelerating builds in sandboxes for expert users.\r\n    .\r\n    If disabled we will not supply instances of `FunctorWithIndex`\r\n    .\r\n  default: True\r\n  manual: True\r\n\r\n\r\nsource-repository head\r\n  type: git\r\n  location: git://github.com/ekmett/comonad.git\r\n\r\nlibrary\r\n  hs-source-dirs: src\r\n  default-language: Haskell2010\r\n  ghc-options: -Wall\r\n\r\n  build-depends:\r\n    base                >= 4   && < 5,\r\n    tagged              >= 0.8.6.1 && < 1,\r\n    transformers        >= 0.3 && < 0.7,\r\n    transformers-compat >= 0.5 && < 1\r\n\r\n  if !impl(ghc >= 8.0)\r\n    build-depends: semigroups >= 0.18.5 && < 1\r\n\r\n  if flag(containers)\r\n    build-depends: containers >= 0.3 && < 0.7\r\n\r\n  if flag(distributive)\r\n    build-depends: distributive >= 0.5.2 && < 1\r\n\r\n  if flag(indexed-traversable)\r\n    build-depends: indexed-traversable >= 0.1.1 && < 0.2\r\n\r\n  if impl(ghc >= 9.0)\r\n    -- these flags may abort compilation with GHC-8.10\r\n    -- https://gitlab.haskell.org/ghc/ghc/-/merge_requests/3295\r\n    ghc-options: -Winferred-safe-imports -Wmissing-safe-haskell-mode\r\n\r\n  exposed-modules:\r\n    Control.Comonad\r\n    Control.Comonad.Env\r\n    Control.Comonad.Env.Class\r\n    Control.Comonad.Hoist.Class\r\n    Control.Comonad.Identity\r\n    Control.Comonad.Store\r\n    Control.Comonad.Store.Class\r\n    Control.Comonad.Traced\r\n    Control.Comonad.Traced.Class\r\n    Control.Comonad.Trans.Class\r\n    Control.Comonad.Trans.Env\r\n    Control.Comonad.Trans.Identity\r\n    Control.Comonad.Trans.Store\r\n    Control.Comonad.Trans.Traced\r\n    Data.Functor.Composition\r\n\r\n  other-extensions:\r\n    CPP\r\n    RankNTypes\r\n    MultiParamTypeClasses\r\n    FunctionalDependencies\r\n    FlexibleInstances\r\n    UndecidableInstances\r\n";
    }