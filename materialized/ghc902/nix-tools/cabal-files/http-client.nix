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
    flags = { network-uri = true; };
    package = {
      specVersion = "1.10";
      identifier = { name = "http-client"; version = "0.7.13.1"; };
      license = "MIT";
      copyright = "";
      maintainer = "michael@snoyman.com";
      author = "Michael Snoyman";
      homepage = "https://github.com/snoyberg/http-client";
      url = "";
      synopsis = "An HTTP client engine";
      description = "Hackage documentation generation is not reliable. For up to date documentation, please see: <http://www.stackage.org/package/http-client>.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = ((([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."http-types" or (errorHandler.buildDepError "http-types"))
          (hsPkgs."blaze-builder" or (errorHandler.buildDepError "blaze-builder"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."network" or (errorHandler.buildDepError "network"))
          (hsPkgs."streaming-commons" or (errorHandler.buildDepError "streaming-commons"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."case-insensitive" or (errorHandler.buildDepError "case-insensitive"))
          (hsPkgs."base64-bytestring" or (errorHandler.buildDepError "base64-bytestring"))
          (hsPkgs."cookie" or (errorHandler.buildDepError "cookie"))
          (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          (hsPkgs."random" or (errorHandler.buildDepError "random"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."mime-types" or (errorHandler.buildDepError "mime-types"))
          (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
          (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
          (hsPkgs."iproute" or (errorHandler.buildDepError "iproute"))
          (hsPkgs."async" or (errorHandler.buildDepError "async"))
          ] ++ (if flags.network-uri
          then [
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            (hsPkgs."network-uri" or (errorHandler.buildDepError "network-uri"))
            ]
          else [
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            ])) ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8.0")) (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))) ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).lt "7.10") (hsPkgs."unsupported-ghc-version" or (errorHandler.buildDepError "unsupported-ghc-version"))) ++ (pkgs.lib).optionals (system.isWindows) [
          (hsPkgs."Win32" or (errorHandler.buildDepError "Win32"))
          (hsPkgs."safe" or (errorHandler.buildDepError "safe"))
          ];
        buildable = true;
        };
      tests = {
        "spec" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."http-client" or (errorHandler.buildDepError "http-client"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."monad-control" or (errorHandler.buildDepError "monad-control"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."http-types" or (errorHandler.buildDepError "http-types"))
            (hsPkgs."blaze-builder" or (errorHandler.buildDepError "blaze-builder"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."case-insensitive" or (errorHandler.buildDepError "case-insensitive"))
            (hsPkgs."zlib" or (errorHandler.buildDepError "zlib"))
            (hsPkgs."async" or (errorHandler.buildDepError "async"))
            (hsPkgs."streaming-commons" or (errorHandler.buildDepError "streaming-commons"))
            ];
          build-tools = [
            (hsPkgs.buildPackages.hspec-discover.components.exes.hspec-discover or (pkgs.buildPackages.hspec-discover or (errorHandler.buildToolDepError "hspec-discover:hspec-discover")))
            ];
          buildable = true;
          };
        "spec-nonet" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."http-client" or (errorHandler.buildDepError "http-client"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."monad-control" or (errorHandler.buildDepError "monad-control"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cookie" or (errorHandler.buildDepError "cookie"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."http-types" or (errorHandler.buildDepError "http-types"))
            (hsPkgs."blaze-builder" or (errorHandler.buildDepError "blaze-builder"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            (hsPkgs."network-uri" or (errorHandler.buildDepError "network-uri"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."case-insensitive" or (errorHandler.buildDepError "case-insensitive"))
            (hsPkgs."zlib" or (errorHandler.buildDepError "zlib"))
            (hsPkgs."async" or (errorHandler.buildDepError "async"))
            (hsPkgs."streaming-commons" or (errorHandler.buildDepError "streaming-commons"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            ];
          build-tools = [
            (hsPkgs.buildPackages.hspec-discover.components.exes.hspec-discover or (pkgs.buildPackages.hspec-discover or (errorHandler.buildToolDepError "hspec-discover:hspec-discover")))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/http-client-0.7.13.1.tar.gz";
      sha256 = "c98e86a0382fa877e320d83dbbd173c41c966a60a32c9bc597058929a7aa0e27";
      });
    }) // {
    package-description-override = "name:                http-client\r\nversion:             0.7.13.1\r\nx-revision: 1\r\nsynopsis:            An HTTP client engine\r\ndescription:         Hackage documentation generation is not reliable. For up to date documentation, please see: <http://www.stackage.org/package/http-client>.\r\nhomepage:            https://github.com/snoyberg/http-client\r\nlicense:             MIT\r\nlicense-file:        LICENSE\r\nauthor:              Michael Snoyman\r\nmaintainer:          michael@snoyman.com\r\ncategory:            Network\r\nbuild-type:          Simple\r\nextra-source-files:  README.md ChangeLog.md\r\ncabal-version:       >=1.10\r\n\r\nflag network-uri\r\n   description: Get Network.URI from the network-uri package\r\n   default: True\r\n\r\nlibrary\r\n  hs-source-dirs:      ., publicsuffixlist\r\n  exposed-modules:     Network.HTTP.Client\r\n                       Network.HTTP.Client.MultipartFormData\r\n                       Network.HTTP.Client.Internal\r\n  other-modules:       Network.HTTP.Client.Body\r\n                       Network.HTTP.Client.Connection\r\n                       Network.HTTP.Client.Cookies\r\n                       Network.HTTP.Client.Core\r\n                       Network.HTTP.Client.Headers\r\n                       Network.HTTP.Client.Manager\r\n                       Network.HTTP.Client.Request\r\n                       Network.HTTP.Client.Response\r\n                       Network.HTTP.Client.Types\r\n                       Network.HTTP.Client.Util\r\n                       Network.HTTP.Proxy\r\n                       Network.PublicSuffixList.Lookup\r\n                       Network.PublicSuffixList.Types\r\n                       Network.PublicSuffixList.Serialize\r\n                       Network.PublicSuffixList.DataStructure\r\n                       Data.KeyedPool\r\n  build-depends:       base              >= 4.10   && < 5\r\n                     , bytestring        >= 0.10\r\n                     , text              >= 0.11\r\n                     , http-types        >= 0.8\r\n                     , blaze-builder     >= 0.3\r\n                     , time              >= 1.2\r\n                     , network           >= 2.4\r\n                     , streaming-commons >= 0.1.0.2 && < 0.3\r\n                     , containers        >= 0.5\r\n                     , transformers\r\n                     , deepseq           >= 1.3    && <1.5\r\n                     , case-insensitive  >= 1.0\r\n                     , base64-bytestring >= 1.0\r\n                     , cookie\r\n                     , exceptions        >= 0.4\r\n                     , array\r\n                     , random\r\n                     , filepath\r\n                     , mime-types\r\n                     , ghc-prim\r\n                     , stm               >= 2.3\r\n                     , iproute           >= 1.7.5\r\n                     , async             >= 2.0\r\n  if flag(network-uri)\r\n    build-depends: network >= 2.6, network-uri >= 2.6\r\n  else\r\n    build-depends: network < 2.6\r\n\r\n  if !impl(ghc>=8.0)\r\n    build-depends: semigroups >= 0.16.1\r\n\r\n  -- See build failure at https://travis-ci.org/snoyberg/http-client/jobs/359573631\r\n  if impl(ghc < 7.10)\r\n    -- Disable building with GHC before 8.0.2.\r\n    -- Due to a cabal bug, do not use buildable: False,\r\n    -- but instead give it an impossible constraint.\r\n    -- See: https://github.com/haskell-infra/hackage-trustees/issues/165\r\n    build-depends: unsupported-ghc-version > 1 && < 1\r\n\r\n\r\n  if os(mingw32)\r\n    build-depends: Win32, safe\r\n\r\n  default-language:    Haskell2010\r\n\r\ntest-suite spec\r\n  main-is:             Spec.hs\r\n  type:                exitcode-stdio-1.0\r\n  hs-source-dirs:      test\r\n  default-language:    Haskell2010\r\n  other-modules:       Network.HTTP.ClientSpec\r\n  build-tool-depends:  hspec-discover:hspec-discover\r\n  build-depends:       base\r\n                     , http-client\r\n                     , hspec\r\n                     , monad-control\r\n                     , bytestring\r\n                     , text\r\n                     , http-types\r\n                     , blaze-builder\r\n                     , time\r\n                     , network\r\n                     , containers\r\n                     , transformers\r\n                     , deepseq\r\n                     , case-insensitive\r\n                     , zlib\r\n                     , async\r\n                     , streaming-commons >= 0.1.1\r\n\r\n\r\ntest-suite spec-nonet\r\n  main-is:             Spec.hs\r\n  type:                exitcode-stdio-1.0\r\n  hs-source-dirs:      test-nonet\r\n  default-language:    Haskell2010\r\n  ghc-options:         -threaded\r\n  if os(windows)\r\n    cpp-options:       -DWINDOWS\r\n  other-modules:       Network.HTTP.ClientSpec\r\n                       Network.HTTP.Client.ResponseSpec\r\n                       Network.HTTP.Client.BodySpec\r\n                       Network.HTTP.Client.HeadersSpec\r\n                       Network.HTTP.Client.RequestSpec\r\n                       Network.HTTP.Client.RequestBodySpec\r\n                       Network.HTTP.Client.CookieSpec\r\n                       Network.HTTP.Client.ConnectionSpec\r\n  build-tool-depends:  hspec-discover:hspec-discover\r\n  build-depends:       base\r\n                     , http-client\r\n                     , hspec\r\n                     , monad-control\r\n                     , bytestring\r\n                     , cookie\r\n                     , text\r\n                     , http-types\r\n                     , blaze-builder\r\n                     , time\r\n                     , network\r\n                     , network-uri\r\n                     , containers\r\n                     , transformers\r\n                     , deepseq\r\n                     , case-insensitive\r\n                     , zlib\r\n                     , async\r\n                     , streaming-commons >= 0.1.1\r\n                     , directory\r\n";
    }