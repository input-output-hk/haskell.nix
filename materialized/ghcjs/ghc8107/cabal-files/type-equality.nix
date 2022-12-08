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
      specVersion = "1.10";
      identifier = { name = "type-equality"; version = "1"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Oleg Grenrus <oleg.grenrus@iki.fi>, Ryan Scott <ryan.gl.scott@gmail.com>, Erik Hesselink <hesselink@gmail.com>";
      author = "Oleg Grenrus <oleg.grenrus@iki.fi>, Ryan Scott <ryan.gl.scott@gmail.com>, Erik Hesselink <hesselink@gmail.com>, Martijn van Steenbergen";
      homepage = "https://github.com/hesselink/type-equality";
      url = "";
      synopsis = "Data.Type.Equality compat package";
      description = "This library defines a propositional equality data type,\nshims @Data.Type.Equality@ as well as possible for older GHCs (< 7.8).\n\n@\ndata a :~: b where\n\\    Refl :: a :~: a\n@\n\nThe module @Data.Type.Equality.Hetero@ shims @:~~:@ equality, for\ncompilers with @PolyKinds@";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [ (hsPkgs."base" or (errorHandler.buildDepError "base")) ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/type-equality-1.tar.gz";
      sha256 = "4728b502a211454ef682a10d7a3e817c22d06ba509df114bb267ef9d43a08ce8";
      });
    }