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
    flags = { deepseq = true; transformers = true; };
    package = {
      specVersion = "1.10";
      identifier = { name = "tagged"; version = "0.8.6.1"; };
      license = "BSD-3-Clause";
      copyright = "2009-2015 Edward A. Kmett";
      maintainer = "Edward A. Kmett <ekmett@gmail.com>";
      author = "Edward A. Kmett";
      homepage = "http://github.com/ekmett/tagged";
      url = "";
      synopsis = "Haskell 98 phantom types to avoid unsafely passing dummy arguments";
      description = "Haskell 98 phantom types to avoid unsafely passing dummy arguments.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = ((([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          ] ++ (pkgs.lib).optional (compiler.isGhc && ((compiler.version).ge "7.2" && (compiler.version).lt "7.5")) (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))) ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).ge "7.6") (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))) ++ (pkgs.lib).optional (flags.deepseq) (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))) ++ (pkgs.lib).optionals (flags.transformers) ([
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          ] ++ (if compiler.isGhc && (compiler.version).ge "7.10" || compiler.isGhcjs && true
          then [
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            ]
          else [
            (hsPkgs."transformers-compat" or (errorHandler.buildDepError "transformers-compat"))
            ]));
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/tagged-0.8.6.1.tar.gz";
      sha256 = "f5e0fcf95f0bb4aa63f428f2c01955a41ea1a42cfcf39145ed631f59a9616c02";
      });
    }) // {
    package-description-override = "name:           tagged\r\nversion:        0.8.6.1\r\nx-revision: 3\r\nlicense:        BSD3\r\nlicense-file:   LICENSE\r\nauthor:         Edward A. Kmett\r\nmaintainer:     Edward A. Kmett <ekmett@gmail.com>\r\nstability:      experimental\r\ncategory:       Data, Phantom Types\r\nsynopsis:       Haskell 98 phantom types to avoid unsafely passing dummy arguments\r\nhomepage:       http://github.com/ekmett/tagged\r\nbug-reports:    http://github.com/ekmett/tagged/issues\r\ncopyright:      2009-2015 Edward A. Kmett\r\ndescription:    Haskell 98 phantom types to avoid unsafely passing dummy arguments.\r\nbuild-type:     Simple\r\ncabal-version:  >= 1.10\r\nextra-source-files: .hlint.yaml CHANGELOG.markdown README.markdown\r\ntested-with:   GHC == 7.0.4\r\n             , GHC == 7.2.2\r\n             , GHC == 7.4.2\r\n             , GHC == 7.6.3\r\n             , GHC == 7.8.4\r\n             , GHC == 7.10.3\r\n             , GHC == 8.0.2\r\n             , GHC == 8.2.2\r\n             , GHC == 8.4.4\r\n             , GHC == 8.6.5\r\n             , GHC == 8.8.3\r\n             , GHC == 8.10.1\r\n\r\nsource-repository head\r\n  type: git\r\n  location: git://github.com/ekmett/tagged.git\r\n\r\nflag deepseq\r\n  description:\r\n    You can disable the use of the `deepseq` package using `-f-deepseq`.\r\n    .\r\n    Disabing this is an unsupported configuration, but it may be useful for accelerating builds in sandboxes for expert users.\r\n  default: True\r\n  manual: True\r\n\r\nflag transformers\r\n  description:\r\n    You can disable the use of the `transformers` and `transformers-compat` packages using `-f-transformers`.\r\n    .\r\n    Disable this is an unsupported configuration, but it may be useful for accelerating builds in sandboxes for expert users.\r\n  default: True\r\n  manual: True\r\n\r\nlibrary\r\n  default-language: Haskell98\r\n  other-extensions: CPP\r\n  build-depends: base >= 2 && < 5\r\n  ghc-options: -Wall\r\n  hs-source-dirs: src\r\n  exposed-modules: Data.Tagged\r\n\r\n  if impl(ghc >= 9.0)\r\n    -- these flags may abort compilation with GHC-8.10\r\n    -- https://gitlab.haskell.org/ghc/ghc/-/merge_requests/3295\r\n    ghc-options: -Winferred-safe-imports -Wmissing-safe-haskell-mode\r\n\r\n  if !impl(hugs)\r\n    cpp-options: -DLANGUAGE_DeriveDataTypeable\r\n    other-extensions: DeriveDataTypeable\r\n\r\n  if impl(ghc<7.7)\r\n    hs-source-dirs: old\r\n    exposed-modules: Data.Proxy\r\n    other-modules: Paths_tagged\r\n\r\n  if impl(ghc>=7.2 && <7.5)\r\n    build-depends: ghc-prim\r\n\r\n  if impl(ghc>=7.6)\r\n    exposed-modules: Data.Proxy.TH\r\n    build-depends: template-haskell >= 2.8 && < 2.20\r\n\r\n  if flag(deepseq)\r\n    build-depends: deepseq >= 1.1 && < 1.5\r\n\r\n  if flag(transformers)\r\n    build-depends: transformers        >= 0.2 && < 0.7\r\n\r\n    -- Ensure Data.Functor.Classes is always available\r\n    if impl(ghc >= 7.10) || impl(ghcjs)\r\n      build-depends: transformers >= 0.4.2.0\r\n    else\r\n      build-depends: transformers-compat >= 0.5 && < 1\r\n";
    }