{ system
  , compiler
  , flags
  , pkgs
  , hsPkgs
  , pkgconfPkgs
  , errorHandler
  , config
  , ... }:
  {
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
    }