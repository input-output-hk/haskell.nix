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
      specVersion = "1.6";
      identifier = { name = "transformers"; version = "0.5.6.2"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Ross Paterson <R.Paterson@city.ac.uk>";
      author = "Andy Gill, Ross Paterson";
      homepage = "";
      url = "";
      synopsis = "Concrete functor and monad transformers";
      description = "A portable library of functor and monad transformers, inspired by\nthe paper\n\n* \\\"Functional Programming with Overloading and Higher-Order\nPolymorphism\\\", by Mark P Jones,\nin /Advanced School of Functional Programming/, 1995\n(<http://web.cecs.pdx.edu/~mpj/pubs/springschool.html>).\n\nThis package contains:\n\n* the monad transformer class (in \"Control.Monad.Trans.Class\")\n\n* concrete functor and monad transformers, each with associated\noperations and functions to lift operations associated with other\ntransformers.\n\nThe package can be used on its own in portable Haskell code, in\nwhich case operations need to be manually lifted through transformer\nstacks (see \"Control.Monad.Trans.Class\" for some examples).\nAlternatively, it can be used with the non-portable monad classes in\nthe @mtl@ or @monads-tf@ packages, which automatically lift operations\nintroduced by monad transformers through other transformers.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          ] ++ (pkgs.lib).optional (compiler.isGhc && ((compiler.version).ge "7.2" && (compiler.version).lt "7.5")) (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"));
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/transformers-0.5.6.2.tar.gz";
      sha256 = "b668795d600297e4c8a7fd55a107b9827b2c52c0bc14c5ea0d65e20e6691c66c";
      });
    }) // {
    package-description-override = "name:         transformers\nversion:      0.5.6.2\nlicense:      BSD3\nlicense-file: LICENSE\nauthor:       Andy Gill, Ross Paterson\nmaintainer:   Ross Paterson <R.Paterson@city.ac.uk>\nbug-reports:  http://hub.darcs.net/ross/transformers/issues\ncategory:     Control\nsynopsis:     Concrete functor and monad transformers\ndescription:\n    A portable library of functor and monad transformers, inspired by\n    the paper\n    .\n    * \\\"Functional Programming with Overloading and Higher-Order\n    Polymorphism\\\", by Mark P Jones,\n    in /Advanced School of Functional Programming/, 1995\n    (<http://web.cecs.pdx.edu/~mpj/pubs/springschool.html>).\n    .\n    This package contains:\n    .\n    * the monad transformer class (in \"Control.Monad.Trans.Class\")\n    .\n    * concrete functor and monad transformers, each with associated\n      operations and functions to lift operations associated with other\n      transformers.\n    .\n    The package can be used on its own in portable Haskell code, in\n    which case operations need to be manually lifted through transformer\n    stacks (see \"Control.Monad.Trans.Class\" for some examples).\n    Alternatively, it can be used with the non-portable monad classes in\n    the @mtl@ or @monads-tf@ packages, which automatically lift operations\n    introduced by monad transformers through other transformers.\nbuild-type: Simple\nextra-source-files:\n    changelog\ncabal-version: >= 1.6\n\nsource-repository head\n  type: darcs\n  location: http://hub.darcs.net/ross/transformers\n\nlibrary\n  build-depends: base >= 2 && < 6\n  hs-source-dirs: .\n  if !impl(ghc>=7.9)\n    -- Data.Functor.Identity was moved into base-4.8.0.0 (GHC 7.10)\n    -- see also https://ghc.haskell.org/trac/ghc/ticket/9664\n    -- NB: using impl(ghc>=7.9) instead of fragile Cabal flags\n    hs-source-dirs: legacy/pre709\n    exposed-modules: Data.Functor.Identity\n  if !impl(ghc>=7.11)\n    -- modules moved into base-4.9.0 (GHC 8.0)\n    -- see https://ghc.haskell.org/trac/ghc/ticket/10773\n    -- see https://ghc.haskell.org/trac/ghc/ticket/11135\n    hs-source-dirs: legacy/pre711\n    exposed-modules:\n      Control.Monad.IO.Class\n      Data.Functor.Classes\n      Data.Functor.Compose\n      Data.Functor.Product\n      Data.Functor.Sum\n  if impl(ghc>=7.2 && <7.5)\n    -- Prior to GHC 7.5, GHC.Generics lived in ghc-prim\n    build-depends: ghc-prim\n  exposed-modules:\n    Control.Applicative.Backwards\n    Control.Applicative.Lift\n    Control.Monad.Signatures\n    Control.Monad.Trans.Accum\n    Control.Monad.Trans.Class\n    Control.Monad.Trans.Cont\n    Control.Monad.Trans.Except\n    Control.Monad.Trans.Error\n    Control.Monad.Trans.Identity\n    Control.Monad.Trans.List\n    Control.Monad.Trans.Maybe\n    Control.Monad.Trans.Reader\n    Control.Monad.Trans.RWS\n    Control.Monad.Trans.RWS.CPS\n    Control.Monad.Trans.RWS.Lazy\n    Control.Monad.Trans.RWS.Strict\n    Control.Monad.Trans.Select\n    Control.Monad.Trans.State\n    Control.Monad.Trans.State.Lazy\n    Control.Monad.Trans.State.Strict\n    Control.Monad.Trans.Writer\n    Control.Monad.Trans.Writer.CPS\n    Control.Monad.Trans.Writer.Lazy\n    Control.Monad.Trans.Writer.Strict\n    Data.Functor.Constant\n    Data.Functor.Reverse\n";
    }