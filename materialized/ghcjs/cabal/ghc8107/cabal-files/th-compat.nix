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
      identifier = { name = "th-compat"; version = "0.1.4"; };
      license = "BSD-3-Clause";
      copyright = "(C) 2020 Ryan Scott";
      maintainer = "Ryan Scott <ryan.gl.scott@gmail.com>";
      author = "Ryan Scott";
      homepage = "https://github.com/haskell-compat/th-compat";
      url = "";
      synopsis = "Backward- (and forward-)compatible Quote and Code types";
      description = "This package defines a \"Language.Haskell.TH.Syntax.Compat\"\nmodule, which backports the @Quote@ and @Code@ types to\nwork across a wide range of @template-haskell@ versions.\nThe @makeRelativeToProject@ utility is also backported.\nOn recent versions of @template-haskell@ (2.17.0.0 or\nlater), this module simply reexports definitions\nfrom \"Language.Haskell.TH.Syntax\". Refer to the Haddocks\nfor \"Language.Haskell.TH.Syntax.Compat\" for examples of\nhow to use this module.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = ([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          ] ++ (pkgs.lib).optionals (!(compiler.isGhc && (compiler.version).ge "8.0")) [
          (hsPkgs."fail" or (errorHandler.buildDepError "fail"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          ]) ++ (pkgs.lib).optionals (!(compiler.isGhc && (compiler.version).ge "9.4")) [
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          ];
        buildable = true;
        };
      tests = {
        "spec" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base-compat" or (errorHandler.buildDepError "base-compat"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
            (hsPkgs."th-compat" or (errorHandler.buildDepError "th-compat"))
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
      url = "http://hackage.haskell.org/package/th-compat-0.1.4.tar.gz";
      sha256 = "d8f97ac14ab47b6b8a7b0fdb4ff95426322ec56badd01652ac15da4a44d4bab8";
      });
    }) // {
    package-description-override = "cabal-version:       >=1.10\r\nname:                th-compat\r\nversion:             0.1.4\r\nx-revision: 3\r\nsynopsis:            Backward- (and forward-)compatible Quote and Code types\r\ndescription:         This package defines a \"Language.Haskell.TH.Syntax.Compat\"\r\n                     module, which backports the @Quote@ and @Code@ types to\r\n                     work across a wide range of @template-haskell@ versions.\r\n                     The @makeRelativeToProject@ utility is also backported.\r\n                     On recent versions of @template-haskell@ (2.17.0.0 or\r\n                     later), this module simply reexports definitions\r\n                     from \"Language.Haskell.TH.Syntax\". Refer to the Haddocks\r\n                     for \"Language.Haskell.TH.Syntax.Compat\" for examples of\r\n                     how to use this module.\r\nhomepage:            https://github.com/haskell-compat/th-compat\r\nbug-reports:         https://github.com/haskell-compat/th-compat/issues\r\nlicense:             BSD3\r\nlicense-file:        LICENSE\r\nauthor:              Ryan Scott\r\nmaintainer:          Ryan Scott <ryan.gl.scott@gmail.com>\r\ncopyright:           (C) 2020 Ryan Scott\r\ncategory:            Text\r\nbuild-type:          Simple\r\ntested-with:         GHC == 7.0.4\r\n                   , GHC == 7.2.2\r\n                   , GHC == 7.4.2\r\n                   , GHC == 7.6.3\r\n                   , GHC == 7.8.4\r\n                   , GHC == 7.10.3\r\n                   , GHC == 8.0.2\r\n                   , GHC == 8.2.2\r\n                   , GHC == 8.4.4\r\n                   , GHC == 8.6.5\r\n                   , GHC == 8.8.4\r\n                   , GHC == 8.10.7\r\n                   , GHC == 9.0.2\r\n                   , GHC == 9.2.3\r\n                   , GHC == 9.4.1\r\nextra-source-files:  CHANGELOG.md, README.md\r\n\r\nsource-repository head\r\n  type:                git\r\n  location:            https://github.com/haskell-compat/th-compat\r\n\r\nlibrary\r\n  exposed-modules:     Language.Haskell.TH.Syntax.Compat\r\n  build-depends:       base             >= 4.3 && < 5\r\n                     , template-haskell >= 2.5 && < 2.22\r\n  if !impl(ghc >= 8.0)\r\n    build-depends:     fail             == 4.9.*\r\n                     , transformers     >= 0.2 && < 0.7\r\n  if !impl(ghc >= 9.4)\r\n    build-depends:     filepath         >= 1.2.0.0 && < 1.5\r\n                     , directory        >= 1.1.0.0 && < 1.4\r\n  hs-source-dirs:      src\r\n  default-language:    Haskell2010\r\n  ghc-options:         -Wall\r\n  if impl(ghc >= 8.6)\r\n    ghc-options:       -Wno-star-is-type\r\n\r\ntest-suite spec\r\n  type:                exitcode-stdio-1.0\r\n  main-is:             Spec.hs\r\n  other-modules:       Language.Haskell.TH.Syntax.CompatSpec\r\n                       Types\r\n  build-depends:       base             >= 4.3 && < 5\r\n                     , base-compat      >= 0.6 && < 0.14\r\n                     , hspec            >= 2   && < 3\r\n                     , mtl              >= 2.1 && < 2.4\r\n                     , template-haskell >= 2.5 && < 2.22\r\n                     , th-compat\r\n  build-tool-depends:  hspec-discover:hspec-discover >= 2\r\n  hs-source-dirs:      tests\r\n  default-language:    Haskell2010\r\n  ghc-options:         -Wall -threaded -rtsopts\r\n";
    }