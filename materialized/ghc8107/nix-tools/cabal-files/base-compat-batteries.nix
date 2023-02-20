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
      identifier = { name = "base-compat-batteries"; version = "0.12.2"; };
      license = "MIT";
      copyright = "(c) 2012-2018 Simon Hengel,\n(c) 2014-2018 João Cristóvão,\n(c) 2015-2018 Ryan Scott";
      maintainer = "Simon Hengel <sol@typeful.net>,\nJoão Cristóvão <jmacristovao@gmail.com>,\nRyan Scott <ryan.gl.scott@gmail.com>";
      author = "Simon Hengel <sol@typeful.net>,\nJoão Cristóvão <jmacristovao@gmail.com>,\nRyan Scott <ryan.gl.scott@gmail.com>";
      homepage = "";
      url = "";
      synopsis = "base-compat with extra batteries";
      description = "Provides functions available in later versions of @base@ to\na wider range of compilers, without requiring you to use CPP\npragmas in your code.\n\nThis package provides the same API as the\n@<http://hackage.haskell.org/package/base-compat base-compat>@\nlibrary, but depends on compatibility packages\n(such as @semigroups@) to offer a wider support window than\n@base-compat@, which has no dependencies. Most of the modules\nin this library have the same names as in @base-compat@\nto make it easier to switch between the two. There also exist\nversions of each module with the suffix @.Repl.Batteries@,\nwhich are distinct from anything in @base-compat@, to allow\nfor easier use in GHCi.\n\nSee\n@<https://github.com/haskell-compat/base-compat/blob/master/base-compat/README.markdown#dependencies here>@\nfor a more comprehensive list of differences between\n@base-compat@ and @base-compat-batteries@.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = ((((([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."base-compat" or (errorHandler.buildDepError "base-compat"))
          (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
          ] ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "7.8")) (hsPkgs."tagged" or (errorHandler.buildDepError "tagged"))) ++ (pkgs.lib).optionals (!(compiler.isGhc && (compiler.version).ge "7.10")) [
          (hsPkgs."nats" or (errorHandler.buildDepError "nats"))
          (hsPkgs."void" or (errorHandler.buildDepError "void"))
          ]) ++ (pkgs.lib).optionals (!(compiler.isGhc && (compiler.version).ge "8.0")) [
          (hsPkgs."fail" or (errorHandler.buildDepError "fail"))
          (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."transformers-compat" or (errorHandler.buildDepError "transformers-compat"))
          ]) ++ (pkgs.lib).optionals (!(compiler.isGhc && (compiler.version).ge "8.2")) [
          (hsPkgs."bifunctors" or (errorHandler.buildDepError "bifunctors"))
          (hsPkgs."type-equality" or (errorHandler.buildDepError "type-equality"))
          ]) ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8.6")) (hsPkgs."contravariant" or (errorHandler.buildDepError "contravariant"))) ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "9.0")) (hsPkgs."OneTuple" or (errorHandler.buildDepError "OneTuple"));
        buildable = true;
        };
      tests = {
        "spec" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base-compat-batteries" or (errorHandler.buildDepError "base-compat-batteries"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            ];
          build-tools = [
            (hsPkgs.buildPackages.hspec-discover.components.exes.hspec-discover or (pkgs.buildPackages.hspec-discover or (errorHandler.buildToolDepError "hspec-discover:hspec-discover")))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/base-compat-batteries-0.12.2.tar.gz";
      sha256 = "ede9092e07f904e0759160bf1ecd3fb7eb043bae6dc89a37c3dc94829ec5eb99";
      });
    }) // {
    package-description-override = "name:             base-compat-batteries\nversion:          0.12.2\nlicense:          MIT\nlicense-file:     LICENSE\ncopyright:        (c) 2012-2018 Simon Hengel,\n                  (c) 2014-2018 João Cristóvão,\n                  (c) 2015-2018 Ryan Scott\nauthor:           Simon Hengel <sol@typeful.net>,\n                  João Cristóvão <jmacristovao@gmail.com>,\n                  Ryan Scott <ryan.gl.scott@gmail.com>\nmaintainer:       Simon Hengel <sol@typeful.net>,\n                  João Cristóvão <jmacristovao@gmail.com>,\n                  Ryan Scott <ryan.gl.scott@gmail.com>\nbuild-type:       Simple\ncabal-version:    >= 1.10\ncategory:         Compatibility\nsynopsis:         base-compat with extra batteries\ndescription:      Provides functions available in later versions of @base@ to\n                  a wider range of compilers, without requiring you to use CPP\n                  pragmas in your code.\n                  .\n                  This package provides the same API as the\n                  @<http://hackage.haskell.org/package/base-compat base-compat>@\n                  library, but depends on compatibility packages\n                  (such as @semigroups@) to offer a wider support window than\n                  @base-compat@, which has no dependencies. Most of the modules\n                  in this library have the same names as in @base-compat@\n                  to make it easier to switch between the two. There also exist\n                  versions of each module with the suffix @.Repl.Batteries@,\n                  which are distinct from anything in @base-compat@, to allow\n                  for easier use in GHCi.\n                  .\n                  See\n                  @<https://github.com/haskell-compat/base-compat/blob/master/base-compat/README.markdown#dependencies here>@\n                  for a more comprehensive list of differences between\n                  @base-compat@ and @base-compat-batteries@.\nextra-source-files: CHANGES.markdown, README.markdown\n\nsource-repository head\n  type: git\n  location: https://github.com/haskell-compat/base-compat\n  subdir: base-compat-batteries\n\nlibrary\n  ghc-options:\n      -Wall\n  default-language:\n      Haskell2010\n  build-depends:\n      base        >= 4.3 && < 5,\n      base-compat == 0.12.2,\n      ghc-prim\n  if !impl(ghc >= 7.8)\n    build-depends:\n      tagged >= 0.8.5 && < 0.9\n  if !impl(ghc >= 7.10)\n    build-depends:\n      nats >= 1.1.2 && < 1.2,\n      void >= 0.7.2 && < 0.8\n  if !impl(ghc >= 8.0)\n    build-depends:\n      fail                >= 4.9.0.0 && < 4.10,\n      semigroups          >= 0.18.4  && < 0.21,\n      transformers        >= 0.2     && < 0.7,\n      transformers-compat >= 0.6     && < 0.8\n  if !impl(ghc >= 8.2)\n    build-depends:\n      bifunctors           >= 5.5.2 && < 5.6,\n      type-equality        >= 1     && < 1.1\n  if !impl(ghc >= 8.6)\n    build-depends:\n      contravariant >= 1.5 && < 1.6\n  if !impl(ghc >= 9.0)\n    build-depends:\n      OneTuple >= 0.3 && < 0.4\n  ghc-options:\n      -fno-warn-duplicate-exports\n  if impl(ghc >= 7.10)\n    ghc-options: -fno-warn-trustworthy-safe\n\n  hs-source-dirs:\n      src\n\n  exposed-modules:\n      Control.Concurrent.Compat\n      Control.Concurrent.MVar.Compat\n      Control.Exception.Compat\n      Control.Monad.Compat\n      Control.Monad.Fail.Compat\n      Control.Monad.IO.Class.Compat\n      Control.Monad.ST.Lazy.Unsafe.Compat\n      Control.Monad.ST.Unsafe.Compat\n      Data.Bifoldable.Compat\n      Data.Bifunctor.Compat\n      Data.Bitraversable.Compat\n      Data.Bits.Compat\n      Data.Bool.Compat\n      Data.Complex.Compat\n      Data.Either.Compat\n      Data.Foldable.Compat\n      Data.Function.Compat\n      Data.Functor.Compat\n      Data.Functor.Compose.Compat\n      Data.Functor.Const.Compat\n      Data.Functor.Contravariant.Compat\n      Data.Functor.Identity.Compat\n      Data.Functor.Product.Compat\n      Data.Functor.Sum.Compat\n      Data.IORef.Compat\n      Data.List.Compat\n      Data.List.NonEmpty.Compat\n      Data.Monoid.Compat\n      Data.Proxy.Compat\n      Data.Ratio.Compat\n      Data.Semigroup.Compat\n      Data.STRef.Compat\n      Data.String.Compat\n      Data.Tuple.Compat\n      Data.Type.Coercion.Compat\n      Data.Type.Equality.Compat\n      Data.Version.Compat\n      Data.Void.Compat\n      Data.Word.Compat\n      Debug.Trace.Compat\n      Foreign.Compat\n      Foreign.ForeignPtr.Compat\n      Foreign.ForeignPtr.Safe.Compat\n      Foreign.ForeignPtr.Unsafe.Compat\n      Foreign.Marshal.Alloc.Compat\n      Foreign.Marshal.Array.Compat\n      Foreign.Marshal.Compat\n      Foreign.Marshal.Safe.Compat\n      Foreign.Marshal.Unsafe.Compat\n      Foreign.Marshal.Utils.Compat\n      Numeric.Compat\n      Numeric.Natural.Compat\n      Prelude.Compat\n      System.Environment.Compat\n      System.Exit.Compat\n      System.IO.Compat\n      System.IO.Error.Compat\n      System.IO.Unsafe.Compat\n      Text.Read.Compat\n      Text.Read.Lex.Compat\n      Type.Reflection.Compat\n\n      Control.Concurrent.Compat.Repl.Batteries\n      Control.Concurrent.MVar.Compat.Repl.Batteries\n      Control.Exception.Compat.Repl.Batteries\n      Control.Monad.Compat.Repl.Batteries\n      Control.Monad.Fail.Compat.Repl.Batteries\n      Control.Monad.IO.Class.Compat.Repl.Batteries\n      Control.Monad.ST.Lazy.Unsafe.Compat.Repl.Batteries\n      Control.Monad.ST.Unsafe.Compat.Repl.Batteries\n      Data.Bifoldable.Compat.Repl.Batteries\n      Data.Bifunctor.Compat.Repl.Batteries\n      Data.Bitraversable.Compat.Repl.Batteries\n      Data.Bits.Compat.Repl.Batteries\n      Data.Bool.Compat.Repl.Batteries\n      Data.Complex.Compat.Repl.Batteries\n      Data.Either.Compat.Repl.Batteries\n      Data.Foldable.Compat.Repl.Batteries\n      Data.Function.Compat.Repl.Batteries\n      Data.Functor.Compat.Repl.Batteries\n      Data.Functor.Compose.Compat.Repl.Batteries\n      Data.Functor.Const.Compat.Repl.Batteries\n      Data.Functor.Identity.Compat.Repl.Batteries\n      Data.Functor.Contravariant.Compat.Repl.Batteries\n      Data.Functor.Product.Compat.Repl.Batteries\n      Data.Functor.Sum.Compat.Repl.Batteries\n      Data.IORef.Compat.Repl.Batteries\n      Data.List.Compat.Repl.Batteries\n      Data.List.NonEmpty.Compat.Repl.Batteries\n      Data.Monoid.Compat.Repl.Batteries\n      Data.Proxy.Compat.Repl.Batteries\n      Data.Ratio.Compat.Repl.Batteries\n      Data.Semigroup.Compat.Repl.Batteries\n      Data.STRef.Compat.Repl.Batteries\n      Data.String.Compat.Repl.Batteries\n      Data.Tuple.Compat.Repl.Batteries\n      Data.Type.Coercion.Compat.Repl.Batteries\n      Data.Type.Equality.Compat.Repl.Batteries\n      Data.Version.Compat.Repl.Batteries\n      Data.Void.Compat.Repl.Batteries\n      Data.Word.Compat.Repl.Batteries\n      Debug.Trace.Compat.Repl.Batteries\n      Foreign.Compat.Repl.Batteries\n      Foreign.ForeignPtr.Compat.Repl.Batteries\n      Foreign.ForeignPtr.Safe.Compat.Repl.Batteries\n      Foreign.ForeignPtr.Unsafe.Compat.Repl.Batteries\n      Foreign.Marshal.Alloc.Compat.Repl.Batteries\n      Foreign.Marshal.Array.Compat.Repl.Batteries\n      Foreign.Marshal.Compat.Repl.Batteries\n      Foreign.Marshal.Safe.Compat.Repl.Batteries\n      Foreign.Marshal.Unsafe.Compat.Repl.Batteries\n      Foreign.Marshal.Utils.Compat.Repl.Batteries\n      Numeric.Compat.Repl.Batteries\n      Numeric.Natural.Compat.Repl.Batteries\n      Prelude.Compat.Repl.Batteries\n      System.Environment.Compat.Repl.Batteries\n      System.Exit.Compat.Repl.Batteries\n      System.IO.Compat.Repl.Batteries\n      System.IO.Error.Compat.Repl.Batteries\n      System.IO.Unsafe.Compat.Repl.Batteries\n      Text.Read.Compat.Repl.Batteries\n      Text.Read.Lex.Compat.Repl.Batteries\n      Type.Reflection.Compat.Repl.Batteries\ntest-suite spec\n  type:\n      exitcode-stdio-1.0\n  ghc-options:\n      -Wall\n  default-language:\n      Haskell2010\n  hs-source-dirs:\n      test\n  main-is:\n      Spec.hs\n  other-modules:\n      Control.Monad.CompatSpec\n      Data.Bits.CompatSpec\n      Data.Bool.CompatSpec\n      Data.Either.CompatSpec\n      Data.Foldable.CompatSpec\n      Data.Function.CompatSpec\n      Data.Functor.CompatSpec\n      Data.IORef.CompatSpec\n      Data.List.CompatSpec\n      Data.Monoid.CompatSpec\n      Data.STRef.CompatSpec\n      Data.Version.CompatSpec\n      Data.Word.CompatSpec\n      Foreign.Marshal.Alloc.CompatSpec\n      Foreign.Marshal.Utils.CompatSpec\n      Numeric.CompatSpec\n      Prelude.CompatSpec\n      System.Environment.CompatSpec\n      Text.Read.CompatSpec\n\n      -- Other tests\n      SafeHaskellSpec\n      TestHspecTrustworthy\n  build-depends:\n      base >= 4.3 && < 5\n    , base-compat-batteries\n    , hspec >= 1.8\n    , QuickCheck\n  build-tool-depends:\n      hspec-discover:hspec-discover >= 1.8\n";
    }