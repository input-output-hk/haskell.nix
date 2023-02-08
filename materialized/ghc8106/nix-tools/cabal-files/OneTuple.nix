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
      identifier = { name = "OneTuple"; version = "0.3.1"; };
      license = "BSD-3-Clause";
      copyright = "(c) John Dorsey 2008";
      maintainer = "Oleg Grenrus <oleg.grenrus@iki.fi>, John Dorsey <haskell@colquitt.org>";
      author = "John Dorsey <haskell@colquitt.org>";
      homepage = "";
      url = "";
      synopsis = "Singleton Tuple";
      description = "This package is a compatibility package for a singleton data type\n\n> data Solo a = Solo a\n\nNote: it's not a @newtype@\n\n@Solo@ is available in @base-4.16@ (GHC-9.2).";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = ((([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          ] ++ (if compiler.isGhc && (compiler.version).ge "9.0"
          then [
            (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
            ]
          else [
            (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
            ])) ++ (pkgs.lib).optionals (!(compiler.isGhc && (compiler.version).ge "8.0")) ([
          (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          ] ++ (if compiler.isGhc && (compiler.version).ge "7.10"
          then [
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            ]
          else [
            (hsPkgs."transformers-compat" or (errorHandler.buildDepError "transformers-compat"))
            ]))) ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "9.2")) (hsPkgs."base-orphans" or (errorHandler.buildDepError "base-orphans"))) ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "7.6")) (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"));
        buildable = true;
        };
      tests = {
        "instances" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
            (hsPkgs."OneTuple" or (errorHandler.buildDepError "OneTuple"))
            ] ++ (pkgs.lib).optionals (!(compiler.isGhc && (compiler.version).ge "8.0")) [
            (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."transformers-compat" or (errorHandler.buildDepError "transformers-compat"))
            ];
          buildable = true;
          };
        "th" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."OneTuple" or (errorHandler.buildDepError "OneTuple"))
            (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/OneTuple-0.3.1.tar.gz";
      sha256 = "98853682d52fb4cc37a45cd186fbd77cf2565d3df5171acc4cf026427e103eef";
      });
    }) // {
    package-description-override = "cabal-version:      >=1.10\nname:               OneTuple\nversion:            0.3.1\nx-revision:         3\nsynopsis:           Singleton Tuple\ncategory:           Data\ndescription:\n  This package is a compatibility package for a singleton data type\n  .\n  > data Solo a = Solo a\n  .\n  Note: it's not a @newtype@\n  .\n  @Solo@ is available in @base-4.16@ (GHC-9.2).\n\ncopyright:          (c) John Dorsey 2008\nlicense:            BSD3\nlicense-file:       LICENSE\nauthor:             John Dorsey <haskell@colquitt.org>\nmaintainer:\n  Oleg Grenrus <oleg.grenrus@iki.fi>, John Dorsey <haskell@colquitt.org>\n\nstability:          experimental\nbuild-type:         Simple\ntested-with:\n  GHC ==7.4.2\n   || ==7.6.3\n   || ==7.8.4\n   || ==7.10.3\n   || ==8.0.2\n   || ==8.2.2\n   || ==8.4.4\n   || ==8.6.5\n   || ==8.8.4\n   || ==8.10.4\n   || ==9.0.2\n   || ==9.2.4\n   || ==9.4.1\n\nextra-source-files: Changelog.md\n\nsource-repository head\n  type:     git\n  location: https://github.com/phadej/OneTuple.git\n\nlibrary\n  default-language: Haskell98\n  exposed-modules:\n    Data.Tuple.OneTuple\n    Data.Tuple.Solo\n    Data.Tuple.Solo.TH\n\n  hs-source-dirs:   src\n  build-depends:\n      base              >=4.5 && <4.18\n    , template-haskell\n\n  if impl(ghc >=9.0)\n    build-depends: ghc-prim\n\n  else\n    build-depends: hashable >=1.3.5.0 && <1.5\n\n  if !impl(ghc >=8.0)\n    build-depends:\n        semigroups    >=0.18.4 && <0.21\n      , transformers  >=0.3    && <0.7\n\n    -- Ensure Data.Functor.Classes is always available\n    if impl(ghc >=7.10)\n      build-depends: transformers >=0.4.2.0\n\n    else\n      build-depends: transformers-compat >=0.5.1.0 && <0.8\n\n  if !impl(ghc >=9.2)\n    build-depends: base-orphans >=0.8.6\n\n  if !impl(ghc >=7.6)\n    build-depends: ghc-prim\n\ntest-suite instances\n  type:             exitcode-stdio-1.0\n  default-language: Haskell98\n  hs-source-dirs:   test\n  main-is:          instances.hs\n  build-depends:\n      base\n    , hashable  >=1.3.5.0 && <1.5\n    , OneTuple\n\n  if !impl(ghc >=8.0)\n    build-depends:\n        semigroups\n      , transformers\n      , transformers-compat\n\ntest-suite th\n  type:             exitcode-stdio-1.0\n  default-language: Haskell98\n  hs-source-dirs:   test\n  main-is:          th.hs\n  build-depends:\n      base\n    , OneTuple\n    , template-haskell\n";
    }