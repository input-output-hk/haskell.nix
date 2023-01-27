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
    package-description-override = "cabal-version:      2.4\nname:               ghc-bignum-orphans\nversion:            0.1.1\nsynopsis:           Backwards-compatible orphan instances for ghc-bignum\ndescription:        @ghc-bignum-orphans@ defines orphan instances that mimic\n                    instances available in later versions of @ghc-bignum@ to a\n                    wider (older) range of compilers. @ghc-bignum-orphans@ does\n                    not export anything except the orphan instances themselves.\nhomepage:           https://github.com/haskell-compat/ghc-bignum-orphans\nbug-reports:        https://github.com/haskell-compat/ghc-bignum-orphans/issues\nlicense:            BSD-3-Clause\nauthor:             Ryan Scott\nmaintainer:         Ryan Scott <ryan.gl.scott@gmail.com>\ncopyright:          (C) 2021 Ryan Scott\ncategory:           Compatibility\nextra-source-files: CHANGELOG.md, README.md\ntested-with:        GHC == 9.0.1\n                     || == 9.2.1\n\nsource-repository head\n  type:               git\n  location:           https://github.com/haskell-compat/ghc-bignum-orphans\n\nlibrary\n    exposed-modules:  GHC.Num.Orphans\n    build-depends:    base       >= 4.15 && < 5\n                    , ghc-bignum >= 1.0  && < 1.3\n    hs-source-dirs:   src\n    default-language: Haskell2010\n    ghc-options:      -Wall -Wcompat\n";
    }