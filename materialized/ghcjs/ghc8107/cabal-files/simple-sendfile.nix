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
    flags = { allow-bsd = true; };
    package = {
      specVersion = "1.10";
      identifier = { name = "simple-sendfile"; version = "0.2.30"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Kazu Yamamoto <kazu@iij.ad.jp>";
      author = "Kazu Yamamoto <kazu@iij.ad.jp>";
      homepage = "";
      url = "";
      synopsis = "Cross platform library for the sendfile system call";
      description = "Cross platform library for the sendfile system call.\nThis library tries to call minimum system calls which\nare the bottleneck of web servers.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."network" or (errorHandler.buildDepError "network"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          ] ++ (if system.isFreebsd && flags.allow-bsd
          then [ (hsPkgs."unix" or (errorHandler.buildDepError "unix")) ]
          else if system.isOsx
            then [ (hsPkgs."unix" or (errorHandler.buildDepError "unix")) ]
            else if system.isLinux
              then [ (hsPkgs."unix" or (errorHandler.buildDepError "unix")) ]
              else [
                (hsPkgs."conduit" or (errorHandler.buildDepError "conduit"))
                (hsPkgs."conduit-extra" or (errorHandler.buildDepError "conduit-extra"))
                (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
                (hsPkgs."resourcet" or (errorHandler.buildDepError "resourcet"))
                ]);
        buildable = true;
        };
      tests = {
        "spec" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."conduit" or (errorHandler.buildDepError "conduit"))
            (hsPkgs."conduit-extra" or (errorHandler.buildDepError "conduit-extra"))
            (hsPkgs."resourcet" or (errorHandler.buildDepError "resourcet"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            (hsPkgs."process" or (errorHandler.buildDepError "process"))
            (hsPkgs."simple-sendfile" or (errorHandler.buildDepError "simple-sendfile"))
            (hsPkgs."unix" or (errorHandler.buildDepError "unix"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/simple-sendfile-0.2.30.tar.gz";
      sha256 = "b6864d2b3c62ff8ea23fa24e9e26f751bfe5253c8efb1f1e4fee2ba91d065284";
      });
    }