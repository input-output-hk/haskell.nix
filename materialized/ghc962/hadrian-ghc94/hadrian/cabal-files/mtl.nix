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
      identifier = { name = "mtl"; version = "2.2.2"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Edward Kmett <ekmett@gmail.com>";
      author = "Andy Gill";
      homepage = "http://github.com/haskell/mtl";
      url = "";
      synopsis = "Monad classes, using functional dependencies";
      description = "Monad classes using functional dependencies, with instances\nfor various monad transformers, inspired by the paper\n/Functional Programming with Overloading and Higher-Order Polymorphism/,\nby Mark P Jones, in /Advanced School of Functional Programming/, 1995\n(<http://web.cecs.pdx.edu/~mpj/pubs/springschool.html>).";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/mtl-2.2.2.tar.gz";
      sha256 = "8803f48a8ed33296c3a3272f448198737a287ec31baa901af09e2118c829bef6";
      });
    }) // {
    package-description-override = "name:         mtl\nversion:      2.2.2\ncabal-version: >= 1.10\nlicense:      BSD3\nlicense-file: LICENSE\nauthor:       Andy Gill\nmaintainer:   Edward Kmett <ekmett@gmail.com>\ncategory:     Control\nsynopsis:     Monad classes, using functional dependencies\nhomepage:     http://github.com/haskell/mtl\nbug-reports:  http://github.com/haskell/mtl/issues\ndescription:\n    Monad classes using functional dependencies, with instances\n    for various monad transformers, inspired by the paper\n    /Functional Programming with Overloading and Higher-Order Polymorphism/,\n    by Mark P Jones, in /Advanced School of Functional Programming/, 1995\n    (<http://web.cecs.pdx.edu/~mpj/pubs/springschool.html>).\nbuild-type: Simple\nextra-source-files: CHANGELOG.markdown, README.markdown\ntested-with:\n  GHC==7.0.4,\n  GHC==7.2.2,\n  GHC==7.4.2,\n  GHC==7.6.3,\n  GHC==7.8.4,\n  GHC==7.10.3,\n  GHC==8.0.2,\n  GHC==8.2.2,\n  GHC==8.4.1\n\nsource-repository head\n  type: git\n  location: https://github.com/haskell/mtl.git\n\nLibrary\n  exposed-modules:\n    Control.Monad.Cont\n    Control.Monad.Cont.Class\n    Control.Monad.Error\n    Control.Monad.Error.Class\n    Control.Monad.Except\n    Control.Monad.Identity\n    Control.Monad.List\n    Control.Monad.RWS\n    Control.Monad.RWS.Class\n    Control.Monad.RWS.Lazy\n    Control.Monad.RWS.Strict\n    Control.Monad.Reader\n    Control.Monad.Reader.Class\n    Control.Monad.State\n    Control.Monad.State.Class\n    Control.Monad.State.Lazy\n    Control.Monad.State.Strict\n    Control.Monad.Trans\n    Control.Monad.Writer\n    Control.Monad.Writer.Class\n    Control.Monad.Writer.Lazy\n    Control.Monad.Writer.Strict\n  build-depends: base < 5, transformers >= 0.4 && <0.6\n\n  default-language: Haskell2010\n  other-extensions:\n    CPP\n    MultiParamTypeClasses\n    FunctionalDependencies\n    FlexibleInstances\n    UndecidableInstances\n\n  -- This is a SafeHaskell safeguard (pun intended) to explicitly declare the API contract of `mtl`\n  -- GHC versions before 7.4 were hopelessly broken or incapable of SafeHaskell\n  if impl(ghc >= 7.4)\n    default-extensions: Safe\n\n  ghc-options: -Wall -fno-warn-unused-imports -fno-warn-warnings-deprecations\n\n  if impl(ghc >= 8.0)\n    ghc-options: -Wcompat -Wnoncanonical-monad-instances -Wnoncanonical-monadfail-instances\n";
    }