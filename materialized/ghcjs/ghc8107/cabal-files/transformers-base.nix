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
    flags = { orphaninstances = true; };
    package = {
      specVersion = "1.8";
      identifier = { name = "transformers-base"; version = "0.4.5.2"; };
      license = "BSD-3-Clause";
      copyright = "2011 Mikhail Vorozhtsov <mikhail.vorozhtsov@gmail.com>,\nBas van Dijk <v.dijk.bas@gmail.com>";
      maintainer = "Mikhail Vorozhtsov <mikhail.vorozhtsov@gmail.com>";
      author = "Mikhail Vorozhtsov <mikhail.vorozhtsov@gmail.com>,\nBas van Dijk <v.dijk.bas@gmail.com>";
      homepage = "https://github.com/mvv/transformers-base";
      url = "";
      synopsis = "Lift computations from the bottom of a transformer stack";
      description = "This package provides a straightforward port of @monadLib@'s BaseM\ntypeclass to @transformers@.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."transformers-compat" or (errorHandler.buildDepError "transformers-compat"))
          ] ++ (pkgs.lib).optional (flags.orphaninstances) (hsPkgs."base-orphans" or (errorHandler.buildDepError "base-orphans"));
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/transformers-base-0.4.5.2.tar.gz";
      sha256 = "d0c80c63fdce6a077dd8eda4f1ff289b85578703a3f1272e141d400fe23245e8";
      });
    }