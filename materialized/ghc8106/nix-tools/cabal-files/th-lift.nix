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
    package-description-override = "Name:               th-lift\r\nVersion:            0.8.2\r\nx-revision: 2\r\nCabal-Version:      1.12\r\nLicense:            BSD3\r\nLicense-Files:      COPYING, BSD3, GPL-2\r\nCopyright:          © 2006 Ian Lynagh, © 2010-2019 Mathieu Boespflug, © 2019 Ryan Scott\r\nAuthor:             Ian Lynagh\r\nMaintainer:         Ryan Scott <ryan.gl.scott@gmail.com>\r\nHomepage:           http://github.com/RyanGlScott/th-lift\r\nSynopsis:           Derive Template Haskell's Lift class for datatypes.\r\nDescription:\r\n  Derive Template Haskell's @Lift@ class for datatypes using @TemplateHaskell@.\r\n  The functionality in this package has largely been subsumed by the\r\n  @DeriveLift@ language extension, which is available in GHC 8.0 and later\r\n  versions. This package can still be useful as a uniform way to derive\r\n  @Lift@ instances that is backwards-compatible with older GHCs.\r\n  .\r\n  The following libraries are related:\r\n  .\r\n  * The <https://hackage.haskell.org/package/th-orphans th-orphans> package\r\n    provides instances for @template-haskell@ syntax types.\r\n  .\r\n  * The <http://hackage.haskell.org/package/th-lift-instances th-lift-instances>\r\n    package provides @Lift@ instances for types in @base@, @text@,\r\n    @bytestring@, @vector@, etc. Some of these instances are only provided for\r\n    old versions of their respective libraries, as the same @Lift@ instances\r\n    are also present upstream on newer versions.\r\nCategory:           Language\r\nTested-With:        GHC==7.0.4, GHC==7.2.2, GHC==7.4.2, GHC==7.6.3, GHC==7.8.4, GHC==7.10.3, GHC==8.0.2, GHC==8.2.2, GHC==8.4.4, GHC==8.6.5, GHC==8.8.3, GHC==8.10.1\r\nbuild-type:         Simple\r\nExtra-source-files: CHANGELOG.md\r\n\r\nsource-repository head\r\n  type:     git\r\n  location: https://github.com/RyanGlScott/th-lift\r\n\r\nLibrary\r\n  Default-Language: Haskell2010\r\n  Exposed-modules:  Language.Haskell.TH.Lift\r\n                    Language.Haskell.TH.Lift.Internal\r\n  Other-Extensions: CPP,  MagicHash, TypeSynonymInstances, FlexibleInstances\r\n  if impl(ghc >= 8.0)\r\n    Other-Extensions: TemplateHaskellQuotes\r\n  else\r\n    Other-Extensions: TemplateHaskell\r\n  Hs-Source-Dirs:   src\r\n  Build-Depends:    base              >= 4.3  && < 5,\r\n                    ghc-prim,\r\n                    th-abstraction   >= 0.2.3 && < 0.5,\r\n                    template-haskell >= 2.5   && < 2.20\r\n  ghc-options:      -Wall\r\n\r\nTest-Suite test\r\n  Default-Language: Haskell2010\r\n  Type:             exitcode-stdio-1.0\r\n  Main-Is:          Test.hs\r\n  Hs-Source-Dirs:   t\r\n  other-modules:    Foo\r\n  ghc-options:      -Wall\r\n  Build-Depends:    base,\r\n                    ghc-prim,\r\n                    th-lift,\r\n                    template-haskell\r\n\r\n";
    }