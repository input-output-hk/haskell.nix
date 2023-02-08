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
      specVersion = "1.12";
      identifier = { name = "th-lift"; version = "0.8.2"; };
      license = "BSD-3-Clause";
      copyright = "© 2006 Ian Lynagh, © 2010-2019 Mathieu Boespflug, © 2019 Ryan Scott";
      maintainer = "Ryan Scott <ryan.gl.scott@gmail.com>";
      author = "Ian Lynagh";
      homepage = "http://github.com/RyanGlScott/th-lift";
      url = "";
      synopsis = "Derive Template Haskell's Lift class for datatypes.";
      description = "Derive Template Haskell's @Lift@ class for datatypes using @TemplateHaskell@.\nThe functionality in this package has largely been subsumed by the\n@DeriveLift@ language extension, which is available in GHC 8.0 and later\nversions. This package can still be useful as a uniform way to derive\n@Lift@ instances that is backwards-compatible with older GHCs.\n\nThe following libraries are related:\n\n* The <https://hackage.haskell.org/package/th-orphans th-orphans> package\nprovides instances for @template-haskell@ syntax types.\n\n* The <http://hackage.haskell.org/package/th-lift-instances th-lift-instances>\npackage provides @Lift@ instances for types in @base@, @text@,\n@bytestring@, @vector@, etc. Some of these instances are only provided for\nold versions of their respective libraries, as the same @Lift@ instances\nare also present upstream on newer versions.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
          (hsPkgs."th-abstraction" or (errorHandler.buildDepError "th-abstraction"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          ];
        buildable = true;
        };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
            (hsPkgs."th-lift" or (errorHandler.buildDepError "th-lift"))
            (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/th-lift-0.8.2.tar.gz";
      sha256 = "3a5927037a10ae63e605c02228c4027c32b7bab1985ae7b5379e6363b3cd5ce4";
      });
    }) // {
    package-description-override = "Name:               th-lift\nVersion:            0.8.2\nCabal-Version:      1.12\nLicense:            BSD3\nLicense-Files:      COPYING, BSD3, GPL-2\nCopyright:          © 2006 Ian Lynagh, © 2010-2019 Mathieu Boespflug, © 2019 Ryan Scott\nAuthor:             Ian Lynagh\nMaintainer:         Ryan Scott <ryan.gl.scott@gmail.com>\nHomepage:           http://github.com/RyanGlScott/th-lift\nSynopsis:           Derive Template Haskell's Lift class for datatypes.\nDescription:\n  Derive Template Haskell's @Lift@ class for datatypes using @TemplateHaskell@.\n  The functionality in this package has largely been subsumed by the\n  @DeriveLift@ language extension, which is available in GHC 8.0 and later\n  versions. This package can still be useful as a uniform way to derive\n  @Lift@ instances that is backwards-compatible with older GHCs.\n  .\n  The following libraries are related:\n  .\n  * The <https://hackage.haskell.org/package/th-orphans th-orphans> package\n    provides instances for @template-haskell@ syntax types.\n  .\n  * The <http://hackage.haskell.org/package/th-lift-instances th-lift-instances>\n    package provides @Lift@ instances for types in @base@, @text@,\n    @bytestring@, @vector@, etc. Some of these instances are only provided for\n    old versions of their respective libraries, as the same @Lift@ instances\n    are also present upstream on newer versions.\nCategory:           Language\nTested-With:        GHC==7.0.4, GHC==7.2.2, GHC==7.4.2, GHC==7.6.3, GHC==7.8.4, GHC==7.10.3, GHC==8.0.2, GHC==8.2.2, GHC==8.4.4, GHC==8.6.5, GHC==8.8.3, GHC==8.10.1\nbuild-type:         Simple\nExtra-source-files: CHANGELOG.md\n\nsource-repository head\n  type:     git\n  location: https://github.com/RyanGlScott/th-lift\n\nLibrary\n  Default-Language: Haskell2010\n  Exposed-modules:  Language.Haskell.TH.Lift\n                    Language.Haskell.TH.Lift.Internal\n  Other-Extensions: CPP,  MagicHash, TypeSynonymInstances, FlexibleInstances\n  if impl(ghc >= 8.0)\n    Other-Extensions: TemplateHaskellQuotes\n  else\n    Other-Extensions: TemplateHaskell\n  Hs-Source-Dirs:   src\n  Build-Depends:    base              >= 4.3  && < 5,\n                    ghc-prim,\n                    th-abstraction   >= 0.2.3 && < 0.5,\n                    template-haskell >= 2.5   && < 2.18\n  ghc-options:      -Wall\n\nTest-Suite test\n  Default-Language: Haskell2010\n  Type:             exitcode-stdio-1.0\n  Main-Is:          Test.hs\n  Hs-Source-Dirs:   t\n  other-modules:    Foo\n  ghc-options:      -Wall\n  Build-Depends:    base,\n                    ghc-prim,\n                    th-lift,\n                    template-haskell\n\n";
    }