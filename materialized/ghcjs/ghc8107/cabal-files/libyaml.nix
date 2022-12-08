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
    flags = { no-unicode = false; system-libyaml = false; };
    package = {
      specVersion = "1.12";
      identifier = { name = "libyaml"; version = "0.1.2"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Michael Snoyman <michael@snoyman.com>";
      author = "Michael Snoyman <michael@snoyman.com>, Anton Ageev <antage@gmail.com>,Kirill Simonov";
      homepage = "https://github.com/snoyberg/yaml#readme";
      url = "";
      synopsis = "Low-level, streaming YAML interface.";
      description = "README and API documentation are available at <https://www.stackage.org/package/libyaml>";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."conduit" or (errorHandler.buildDepError "conduit"))
          (hsPkgs."resourcet" or (errorHandler.buildDepError "resourcet"))
          ] ++ (pkgs.lib).optional (system.isWindows) (hsPkgs."directory" or (errorHandler.buildDepError "directory"));
        libs = (pkgs.lib).optional (!(!flags.system-libyaml)) (pkgs."yaml" or (errorHandler.sysDepError "yaml"));
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/libyaml-0.1.2.tar.gz";
      sha256 = "8f42d66f199fcaee255326f8f770d88b0670df56b5eb78002d6058f3a45e97b5";
      });
    }