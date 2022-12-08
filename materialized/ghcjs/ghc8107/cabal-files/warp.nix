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
    flags = {
      network-bytestring = false;
      allow-sendfilefd = true;
      warp-debug = false;
      };
    package = {
      specVersion = "1.10";
      identifier = { name = "warp"; version = "3.3.14"; };
      license = "MIT";
      copyright = "";
      maintainer = "michael@snoyman.com";
      author = "Michael Snoyman, Kazu Yamamoto, Matt Brown";
      homepage = "http://github.com/yesodweb/wai";
      url = "";
      synopsis = "A fast, light-weight web server for WAI applications.";
      description = "HTTP\\/1.0, HTTP\\/1.1 and HTTP\\/2 are supported.\nFor HTTP\\/2,  Warp supports direct and ALPN (in TLS)\nbut not upgrade.\nAPI docs and the README are available at\n<http://www.stackage.org/package/warp>.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = (([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          (hsPkgs."async" or (errorHandler.buildDepError "async"))
          (hsPkgs."auto-update" or (errorHandler.buildDepError "auto-update"))
          (hsPkgs."bsb-http-chunked" or (errorHandler.buildDepError "bsb-http-chunked"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."case-insensitive" or (errorHandler.buildDepError "case-insensitive"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."http-date" or (errorHandler.buildDepError "http-date"))
          (hsPkgs."http-types" or (errorHandler.buildDepError "http-types"))
          (hsPkgs."http2" or (errorHandler.buildDepError "http2"))
          (hsPkgs."iproute" or (errorHandler.buildDepError "iproute"))
          (hsPkgs."simple-sendfile" or (errorHandler.buildDepError "simple-sendfile"))
          (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
          (hsPkgs."streaming-commons" or (errorHandler.buildDepError "streaming-commons"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time-manager" or (errorHandler.buildDepError "time-manager"))
          (hsPkgs."unix-compat" or (errorHandler.buildDepError "unix-compat"))
          (hsPkgs."vault" or (errorHandler.buildDepError "vault"))
          (hsPkgs."wai" or (errorHandler.buildDepError "wai"))
          (hsPkgs."word8" or (errorHandler.buildDepError "word8"))
          (hsPkgs."x509" or (errorHandler.buildDepError "x509"))
          ] ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).lt "8") (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))) ++ (if flags.network-bytestring
          then [
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            (hsPkgs."network-bytestring" or (errorHandler.buildDepError "network-bytestring"))
            ]
          else [
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            ])) ++ (if system.isWindows
          then [ (hsPkgs."time" or (errorHandler.buildDepError "time")) ]
          else [ (hsPkgs."unix" or (errorHandler.buildDepError "unix")) ]);
        buildable = true;
        };
      tests = {
        "doctest" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."doctest" or (errorHandler.buildDepError "doctest"))
            ];
          buildable = false;
          };
        "spec" = {
          depends = (([
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."array" or (errorHandler.buildDepError "array"))
            (hsPkgs."async" or (errorHandler.buildDepError "async"))
            (hsPkgs."auto-update" or (errorHandler.buildDepError "auto-update"))
            (hsPkgs."bsb-http-chunked" or (errorHandler.buildDepError "bsb-http-chunked"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."case-insensitive" or (errorHandler.buildDepError "case-insensitive"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
            (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."http-client" or (errorHandler.buildDepError "http-client"))
            (hsPkgs."http-date" or (errorHandler.buildDepError "http-date"))
            (hsPkgs."http-types" or (errorHandler.buildDepError "http-types"))
            (hsPkgs."http2" or (errorHandler.buildDepError "http2"))
            (hsPkgs."iproute" or (errorHandler.buildDepError "iproute"))
            (hsPkgs."lifted-base" or (errorHandler.buildDepError "lifted-base"))
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            (hsPkgs."process" or (errorHandler.buildDepError "process"))
            (hsPkgs."simple-sendfile" or (errorHandler.buildDepError "simple-sendfile"))
            (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
            (hsPkgs."streaming-commons" or (errorHandler.buildDepError "streaming-commons"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."time-manager" or (errorHandler.buildDepError "time-manager"))
            (hsPkgs."unix-compat" or (errorHandler.buildDepError "unix-compat"))
            (hsPkgs."vault" or (errorHandler.buildDepError "vault"))
            (hsPkgs."wai" or (errorHandler.buildDepError "wai"))
            (hsPkgs."word8" or (errorHandler.buildDepError "word8"))
            (hsPkgs."x509" or (errorHandler.buildDepError "x509"))
            ] ++ (pkgs.lib).optionals (compiler.isGhc && (compiler.version).lt "8") [
            (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            ]) ++ (pkgs.lib).optional ((system.isLinux || system.isFreebsd || system.isOsx) && flags.allow-sendfilefd) (hsPkgs."unix" or (errorHandler.buildDepError "unix"))) ++ (pkgs.lib).optional (system.isWindows) (hsPkgs."time" or (errorHandler.buildDepError "time"));
          buildable = true;
          };
        };
      benchmarks = {
        "parser" = {
          depends = (([
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."auto-update" or (errorHandler.buildDepError "auto-update"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."gauge" or (errorHandler.buildDepError "gauge"))
            (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
            (hsPkgs."http-date" or (errorHandler.buildDepError "http-date"))
            (hsPkgs."http-types" or (errorHandler.buildDepError "http-types"))
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            (hsPkgs."time-manager" or (errorHandler.buildDepError "time-manager"))
            (hsPkgs."unix-compat" or (errorHandler.buildDepError "unix-compat"))
            (hsPkgs."x509" or (errorHandler.buildDepError "x509"))
            ] ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).lt "8") (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))) ++ (pkgs.lib).optional ((system.isLinux || system.isFreebsd || system.isOsx) && flags.allow-sendfilefd) (hsPkgs."unix" or (errorHandler.buildDepError "unix"))) ++ (pkgs.lib).optional (system.isWindows) (hsPkgs."time" or (errorHandler.buildDepError "time"));
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/warp-3.3.14.tar.gz";
      sha256 = "2331da1ac67c644828883498301bee7bbf59f8b3d79b37850a621cba9a811572";
      });
    }