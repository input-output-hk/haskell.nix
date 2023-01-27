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
    flags = { werror = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "dlist"; version = "1.0"; };
      license = "BSD-3-Clause";
      copyright = "2006-2009 Don Stewart, 2013-2020 Sean Leather, 2017-2020 Oleg Grenrus, contributors";
      maintainer = "Sean Leather <sean.leather@gmail.com>";
      author = "Don Stewart";
      homepage = "https://github.com/spl/dlist";
      url = "";
      synopsis = "Difference lists";
      description = "List-like types supporting O(1) append and snoc operations.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          ];
        buildable = true;
        };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."dlist" or (errorHandler.buildDepError "dlist"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/dlist-1.0.tar.gz";
      sha256 = "173d637328bb173fcc365f30d29ff4a94292a1e0e5558aeb3dfc11de81510115";
      });
    }) // {
    package-description-override = "cabal-version:          >= 1.10\n\nname:                   dlist\nversion:                1.0\nsynopsis:               Difference lists\ndescription:\n  List-like types supporting O(1) append and snoc operations.\ncategory:               Data\nlicense:                BSD3\nlicense-file:           license.md\nauthor:                 Don Stewart\nmaintainer:             Sean Leather <sean.leather@gmail.com>\ncopyright:              2006-2009 Don Stewart, 2013-2020 Sean Leather, 2017-2020 Oleg Grenrus, contributors\nhomepage:               https://github.com/spl/dlist\nbug-reports:            https://github.com/spl/dlist/issues\nextra-source-files:     readme.md,\n                        changelog.md\n                        tests/ImportUnsafe.hs\nbuild-type:             Simple\ntested-with:            GHC==7.0.4\n                        GHC==7.2.2\n                        GHC==7.4.2\n                        GHC==7.6.3\n                        GHC==7.8.4\n                        GHC==7.10.3\n                        GHC==8.0.2\n                        GHC==8.2.2\n                        GHC==8.4.4\n                        GHC==8.6.5\n                        GHC==8.8.3\n                        GHC==8.10.1\n\nsource-repository head\n  type:                 git\n  location:             git://github.com/spl/dlist.git\n\nflag Werror\n  description:          Enable -Werror\n  default:              False\n  manual:               True\n\nlibrary\n  build-depends:\n                        base >= 4 && < 5,\n                        deepseq >= 1.1 && < 1.5\n  exposed-modules:      Data.DList\n                        Data.DList.Unsafe\n  other-modules:        Data.DList.Internal\n  if impl(ghc >= 8.0)\n    exposed-modules:    Data.DList.DNonEmpty\n    other-modules:      Data.DList.DNonEmpty.Internal\n  default-language:     Haskell2010\n  ghc-options:          -Wall\n  if impl(ghc >= 8.0)\n    ghc-options:        -Wcompat\n                        -Wincomplete-record-updates\n                        -Wincomplete-uni-patterns\n                        -Wnoncanonical-monad-instances\n  if impl(ghc >= 8.2)\n    ghc-options:        -Wmissing-home-modules\n  if impl(ghc >= 8.4)\n    ghc-options:        -Wpartial-fields\n  if impl(ghc >= 8.10)\n    ghc-options:        -Wmissing-safe-haskell-mode\n                        -Wtrustworthy-safe\n  if flag(Werror)\n    ghc-options:        -Werror\n\ntest-suite test\n  type:                 exitcode-stdio-1.0\n  main-is:              Main.hs\n  other-modules:        DListProperties\n                        OverloadedStrings\n                        QuickCheckUtil\n  if impl(ghc >= 8.0)\n    other-modules:      DNonEmptyProperties\n  hs-source-dirs:       tests\n  build-depends:        dlist,\n                        base,\n                        -- QuickCheck-2.10 is the first version supporting\n                        -- base-4.9 (ghc-8) without the Arbitrary NonEmpty\n                        -- instance, which we include ourselves.\n                        QuickCheck >= 2.10 && < 2.15\n  default-language:     Haskell2010\n  ghc-options:          -Wall\n  if impl(ghc >= 8.0)\n    ghc-options:        -Wcompat\n                        -Wincomplete-record-updates\n                        -Wincomplete-uni-patterns\n                        -Wnoncanonical-monad-instances\n  if impl(ghc >= 8.2)\n    ghc-options:        -Wmissing-home-modules\n  if impl(ghc >= 8.4)\n    ghc-options:        -Wpartial-fields\n  if impl(ghc >= 8.10)\n    ghc-options:        -Wmissing-safe-haskell-mode\n                        -Wtrustworthy-safe\n  if flag(Werror)\n    ghc-options:        -Werror\n";
    }