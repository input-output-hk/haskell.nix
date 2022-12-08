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
      warn-as-error = false;
      conduit10 = false;
      warp-tests = false;
      network-uri = true;
      };
    package = {
      specVersion = "1.10";
      identifier = { name = "HTTP"; version = "4000.4.1"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Ganesh Sittampalam <ganesh@earth.li>";
      author = "Warrick Gray <warrick.gray@hotmail.com>";
      homepage = "https://github.com/haskell/HTTP";
      url = "";
      synopsis = "A library for client-side HTTP";
      description = "The HTTP package supports client-side web programming in Haskell. It lets you set up\nHTTP connections, transmitting requests and processing the responses coming back, all\nfrom within the comforts of Haskell. It's dependent on the network package to operate,\nbut other than that, the implementation is all written in Haskell.\n\nA basic API for issuing single HTTP requests + receiving responses is provided. On top\nof that, a session-level abstraction is also on offer  (the @BrowserAction@ monad);\nit taking care of handling the management of persistent connections, proxies,\nstate (cookies) and authentication credentials required to handle multi-step\ninteractions with a web server.\n\nThe representation of the bytes flowing across is extensible via the use of a type class,\nletting you pick the representation of requests and responses that best fits your use.\nSome pre-packaged, common instances are provided for you (@ByteString@, @String@).\n\nHere's an example use:\n\n>\n>    do\n>      rsp <- Network.HTTP.simpleHTTP (getRequest \"http://www.haskell.org/\")\n>              -- fetch document and return it (as a 'String'.)\n>      fmap (take 100) (getResponseBody rsp)\n>\n>    do\n>      (_, rsp)\n>         <- Network.Browser.browse $ do\n>               setAllowRedirects True -- handle HTTP redirects\n>               request $ getRequest \"http://www.haskell.org/\"\n>      return (take 100 (rspBody rsp))\n\n__Note:__ This package does not support HTTPS connections.\nIf you need HTTPS, take a look at the following packages:\n\n* <http://hackage.haskell.org/package/http-streams http-streams>\n\n* <http://hackage.haskell.org/package/http-client http-client> (in combination with\n<http://hackage.haskell.org/package/http-client-tls http-client-tls>)\n\n* <http://hackage.haskell.org/package/req req>\n\n* <http://hackage.haskell.org/package/wreq wreq>\n";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = ([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."parsec" or (errorHandler.buildDepError "parsec"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."network" or (errorHandler.buildDepError "network"))
          ] ++ (if flags.network-uri
          then [
            (hsPkgs."network-uri" or (errorHandler.buildDepError "network-uri"))
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            ]
          else [
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            ])) ++ (pkgs.lib).optional (system.isWindows) (hsPkgs."Win32" or (errorHandler.buildDepError "Win32"));
        buildable = true;
        };
      tests = {
        "test" = {
          depends = ([
            (hsPkgs."HTTP" or (errorHandler.buildDepError "HTTP"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."httpd-shed" or (errorHandler.buildDepError "httpd-shed"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."pureMD5" or (errorHandler.buildDepError "pureMD5"))
            (hsPkgs."split" or (errorHandler.buildDepError "split"))
            (hsPkgs."test-framework" or (errorHandler.buildDepError "test-framework"))
            (hsPkgs."test-framework-hunit" or (errorHandler.buildDepError "test-framework-hunit"))
            ] ++ (if flags.network-uri
            then [
              (hsPkgs."network-uri" or (errorHandler.buildDepError "network-uri"))
              (hsPkgs."network" or (errorHandler.buildDepError "network"))
              ]
            else [
              (hsPkgs."network" or (errorHandler.buildDepError "network"))
              ])) ++ (pkgs.lib).optionals (flags.warp-tests) ([
            (hsPkgs."case-insensitive" or (errorHandler.buildDepError "case-insensitive"))
            (hsPkgs."conduit" or (errorHandler.buildDepError "conduit"))
            (hsPkgs."http-types" or (errorHandler.buildDepError "http-types"))
            (hsPkgs."wai" or (errorHandler.buildDepError "wai"))
            (hsPkgs."warp" or (errorHandler.buildDepError "warp"))
            ] ++ (if flags.conduit10
            then [
              (hsPkgs."conduit" or (errorHandler.buildDepError "conduit"))
              ]
            else [
              (hsPkgs."conduit" or (errorHandler.buildDepError "conduit"))
              (hsPkgs."conduit-extra" or (errorHandler.buildDepError "conduit-extra"))
              ]));
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/HTTP-4000.4.1.tar.gz";
      sha256 = "df31d8efec775124dab856d7177ddcba31be9f9e0836ebdab03d94392f2dd453";
      });
    }