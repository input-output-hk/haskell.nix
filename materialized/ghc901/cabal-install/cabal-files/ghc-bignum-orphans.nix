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
      specVersion = "2.4";
      identifier = { name = "ghc-bignum-orphans"; version = "0.1.1"; };
      license = "BSD-3-Clause";
      copyright = "(C) 2021 Ryan Scott";
      maintainer = "Ryan Scott <ryan.gl.scott@gmail.com>";
      author = "Ryan Scott";
      homepage = "https://github.com/haskell-compat/ghc-bignum-orphans";
      url = "";
      synopsis = "Backwards-compatible orphan instances for ghc-bignum";
      description = "@ghc-bignum-orphans@ defines orphan instances that mimic\ninstances available in later versions of @ghc-bignum@ to a\nwider (older) range of compilers. @ghc-bignum-orphans@ does\nnot export anything except the orphan instances themselves.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."ghc-bignum" or (errorHandler.buildDepError "ghc-bignum"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/ghc-bignum-orphans-0.1.1.tar.gz";
      sha256 = "a4c617c7b90288ba3e24c67633f99e97e11c2367686463b8884d2cd3591241db";
      });
    }) // {
    package-description-override = "cabal-version:      2.4\r\nname:               ghc-bignum-orphans\r\nversion:            0.1.1\r\nx-revision: 1\r\nsynopsis:           Backwards-compatible orphan instances for ghc-bignum\r\ndescription:        @ghc-bignum-orphans@ defines orphan instances that mimic\r\n                    instances available in later versions of @ghc-bignum@ to a\r\n                    wider (older) range of compilers. @ghc-bignum-orphans@ does\r\n                    not export anything except the orphan instances themselves.\r\nhomepage:           https://github.com/haskell-compat/ghc-bignum-orphans\r\nbug-reports:        https://github.com/haskell-compat/ghc-bignum-orphans/issues\r\nlicense:            BSD-3-Clause\r\nauthor:             Ryan Scott\r\nmaintainer:         Ryan Scott <ryan.gl.scott@gmail.com>\r\ncopyright:          (C) 2021 Ryan Scott\r\ncategory:           Compatibility\r\nextra-source-files: CHANGELOG.md, README.md\r\ntested-with:        GHC == 9.0.1\r\n                     || == 9.2.1\r\n\r\nsource-repository head\r\n  type:               git\r\n  location:           https://github.com/haskell-compat/ghc-bignum-orphans\r\n\r\nlibrary\r\n    exposed-modules:  GHC.Num.Orphans\r\n    build-depends:    base       >= 4.15 && < 5\r\n                    , ghc-bignum >= 1.0  && < 1.4\r\n    hs-source-dirs:   src\r\n    default-language: Haskell2010\r\n    ghc-options:      -Wall -Wcompat\r\n";
    }