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
    flags = { semigroups = true; tagged = true; };
    package = {
      specVersion = "1.10";
      identifier = { name = "distributive"; version = "0.6.2.1"; };
      license = "BSD-3-Clause";
      copyright = "Copyright (C) 2011-2016 Edward A. Kmett";
      maintainer = "Edward A. Kmett <ekmett@gmail.com>";
      author = "Edward A. Kmett";
      homepage = "http://github.com/ekmett/distributive/";
      url = "";
      synopsis = "Distributive functors -- Dual to Traversable";
      description = "Distributive functors -- Dual to @Traversable@";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = (([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."base-orphans" or (errorHandler.buildDepError "base-orphans"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          ] ++ (pkgs.lib).optional (flags.tagged) (hsPkgs."tagged" or (errorHandler.buildDepError "tagged"))) ++ (pkgs.lib).optional (compiler.isGhc && ((compiler.version).ge "7.2" && (compiler.version).lt "7.6")) (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))) ++ (pkgs.lib).optionals (compiler.isGhc && (compiler.version).lt "8.0") ((pkgs.lib).optional (flags.semigroups) (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups")));
        buildable = true;
        };
      tests = {
        "spec" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."distributive" or (errorHandler.buildDepError "distributive"))
            (hsPkgs."generic-deriving" or (errorHandler.buildDepError "generic-deriving"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
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
      url = "http://hackage.haskell.org/package/distributive-0.6.2.1.tar.gz";
      sha256 = "d7351392e078f58caa46630a4b9c643e1e2e9dddee45848c5c8358e7b1316b91";
      });
    }) // {
    package-description-override = "name:          distributive\r\ncategory:      Data Structures\r\nversion:       0.6.2.1\r\nx-revision: 1\r\nlicense:       BSD3\r\ncabal-version: >= 1.10\r\nlicense-file:  LICENSE\r\nauthor:        Edward A. Kmett\r\nmaintainer:    Edward A. Kmett <ekmett@gmail.com>\r\nstability:     provisional\r\nhomepage:      http://github.com/ekmett/distributive/\r\nbug-reports:   http://github.com/ekmett/distributive/issues\r\ncopyright:     Copyright (C) 2011-2016 Edward A. Kmett\r\nsynopsis:      Distributive functors -- Dual to Traversable\r\ndescription:   Distributive functors -- Dual to @Traversable@\r\nbuild-type:    Simple\r\ntested-with:   GHC == 7.0.4\r\n             , GHC == 7.2.2\r\n             , GHC == 7.4.2\r\n             , GHC == 7.6.3\r\n             , GHC == 7.8.4\r\n             , GHC == 7.10.3\r\n             , GHC == 8.0.2\r\n             , GHC == 8.2.2\r\n             , GHC == 8.4.4\r\n             , GHC == 8.6.5\r\n             , GHC == 8.8.4\r\n             , GHC == 8.10.3\r\nextra-source-files:\r\n  .hlint.yaml\r\n  .vim.custom\r\n  config\r\n  CHANGELOG.markdown\r\n  README.markdown\r\n\r\nsource-repository head\r\n  type: git\r\n  location: git://github.com/ekmett/distributive.git\r\n\r\nflag semigroups\r\n  manual: True\r\n  default: True\r\n  description:\r\n    You can disable the use of the `semigroups` package using `-f-semigroups`.\r\n    .\r\n    Disabling this is an unsupported configuration, but it may be useful for accelerating builds in sandboxes for expert users.\r\n\r\nflag tagged\r\n  manual: True\r\n  default: True\r\n  description:\r\n    You can disable the use of the `tagged` package using `-f-tagged`.\r\n    .\r\n    Disabling this is an unsupported configuration, but it may be useful for accelerating builds in sandboxes for expert users.\r\n\r\nlibrary\r\n  build-depends:\r\n    base                >= 4   && < 5,\r\n    base-orphans        >= 0.5.2 && < 1,\r\n    transformers        >= 0.3 && < 0.7\r\n\r\n  hs-source-dirs:  src\r\n  exposed-modules:\r\n    Data.Distributive\r\n\r\n  if impl(ghc>=7.2)\r\n    exposed-modules: Data.Distributive.Generic\r\n\r\n  if flag(tagged)\r\n    build-depends: tagged >= 0.7 && < 1\r\n\r\n  if impl(ghc>=7.2 && < 7.6)\r\n    build-depends: ghc-prim\r\n\r\n  if impl(ghc < 8.0)\r\n    if flag(semigroups)\r\n      build-depends: semigroups >= 0.13 && < 1\r\n\r\n  if impl(ghc < 7.8)\r\n    hs-source-dirs: src-compat\r\n    other-modules: Data.Coerce\r\n\r\n  ghc-options: -Wall\r\n\r\n  if impl(ghc >= 9.0)\r\n    -- these flags may abort compilation with GHC-8.10\r\n    -- https://gitlab.haskell.org/ghc/ghc/-/merge_requests/3295\r\n    ghc-options: -Winferred-safe-imports -Wmissing-safe-haskell-mode\r\n\r\n  default-language: Haskell2010\r\n\r\ntest-suite spec\r\n  type:           exitcode-stdio-1.0\r\n  hs-source-dirs: tests\r\n  build-tool-depends:\r\n    hspec-discover:hspec-discover\r\n\r\n  build-depends:\r\n    base             >= 4    && < 5,\r\n    distributive,\r\n    generic-deriving >= 1.11 && < 2,\r\n    hspec            >= 2    && < 3\r\n\r\n  main-is: Spec.hs\r\n  other-modules: GenericsSpec\r\n\r\n  ghc-options: -Wall -threaded -rtsopts\r\n  default-language: Haskell2010\r\n";
    }