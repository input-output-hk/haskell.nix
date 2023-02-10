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
      specVersion = "1.8";
      identifier = { name = "network"; version = "2.8.0.1"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Kazu Yamamoto, Evan Borden";
      author = "";
      homepage = "https://github.com/haskell/network";
      url = "";
      synopsis = "Low-level networking interface";
      description = "This package provides a low-level networking interface.\n\nIn network-2.6 the @Network.URI@ module was split off into its own\npackage, network-uri-2.6. If you're using the @Network.URI@ module\nyou can automatically get it from the right package by adding this\nto your .cabal file:\n\n> library\n>   build-depends: network-uri-flag";
      buildType = "Configure";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          ] ++ (pkgs.lib).optional (!system.isWindows) (hsPkgs."unix" or (errorHandler.buildDepError "unix"));
        buildable = true;
        };
      tests = {
        "spec" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            ];
          build-tools = [
            (hsPkgs.buildPackages.hspec-discover.components.exes.hspec-discover or (pkgs.buildPackages.hspec-discover or (errorHandler.buildToolDepError "hspec-discover:hspec-discover")))
            ];
          buildable = true;
          };
        "doctest" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."doctest" or (errorHandler.buildDepError "doctest"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/network-2.8.0.1.tar.gz";
      sha256 = "61f55dbfed0f0af721a8ea36079e9309fcc5a1be20783b44ae500d9e4399a846";
      });
    }) // {
    package-description-override = "name:           network\nversion:        2.8.0.1\nlicense:        BSD3\nlicense-file:   LICENSE\nmaintainer:     Kazu Yamamoto, Evan Borden\nsynopsis:       Low-level networking interface\ndescription:\n  This package provides a low-level networking interface.\n  .\n  In network-2.6 the @Network.URI@ module was split off into its own\n  package, network-uri-2.6. If you're using the @Network.URI@ module\n  you can automatically get it from the right package by adding this\n  to your .cabal file:\n  .\n  > library\n  >   build-depends: network-uri-flag\ncategory:       Network\nbuild-type:     Configure\ncabal-version:  >=1.8\nextra-tmp-files:\n  config.log config.status autom4te.cache network.buildinfo\n  include/HsNetworkConfig.h\nextra-source-files:\n  README.md CHANGELOG.md\n  examples/*.hs tests/*.hs config.guess config.sub install-sh\n  configure.ac configure network.buildinfo.in\n  include/HsNetworkConfig.h.in include/HsNet.h include/HsNetDef.h\n  -- C sources only used on some systems\n  cbits/ancilData.c cbits/asyncAccept.c cbits/initWinSock.c\n  cbits/winSockErr.c\nhomepage:       https://github.com/haskell/network\nbug-reports:    https://github.com/haskell/network/issues\ntested-with:   GHC == 7.8.4\n             , GHC == 7.10.3\n             , GHC == 8.0.2\n             , GHC == 8.2.2\n             , GHC == 8.4.3\n\nlibrary\n  exposed-modules:\n    Network\n    Network.BSD\n    Network.Socket\n    Network.Socket.ByteString\n    Network.Socket.ByteString.Lazy\n    Network.Socket.Internal\n  other-modules:\n    Network.Socket.ByteString.Internal\n    Network.Socket.Types\n\n  if !os(windows)\n    other-modules:\n      Network.Socket.ByteString.IOVec\n      Network.Socket.ByteString.Lazy.Posix\n      Network.Socket.ByteString.MsgHdr\n  if os(windows)\n    other-modules:\n      Network.Socket.ByteString.Lazy.Windows\n\n  build-depends:\n    base >= 4.7 && < 5,\n    bytestring == 0.10.*\n\n  if !os(windows)\n    build-depends:\n      unix >= 2\n\n  extensions:\n    CPP, DeriveDataTypeable, ForeignFunctionInterface, TypeSynonymInstances\n  include-dirs: include\n  includes: HsNet.h HsNetDef.h\n  install-includes: HsNet.h HsNetDef.h\n  c-sources: cbits/HsNet.c\n  ghc-options: -Wall -fwarn-tabs\n\ntest-suite spec\n  hs-source-dirs: tests\n  main-is: Spec.hs\n  other-modules:\n    Network.Test.Common\n    Network.SocketSpec\n    Network.Socket.ByteStringSpec\n    Network.Socket.ByteString.LazySpec\n  type: exitcode-stdio-1.0\n  ghc-options: -Wall -threaded\n  -- NB: make sure to versions of hspec and hspec-discover\n  --     that work together; easiest way is to constraint\n  --     both packages to a small enough version range.\n  build-tools: hspec-discover >= 2.6\n  build-depends:\n    base >= 4.7 && < 5,\n    bytestring,\n    directory,\n    HUnit,\n    network,\n    hspec >= 2.6\n\ntest-suite doctest\n  hs-source-dirs: tests\n  main-is: doctests.hs\n  type: exitcode-stdio-1.0\n\n  build-depends:\n    base >= 4.7 && < 5,\n    doctest >= 0.10.1\n\n  ghc-options: -Wall\n\nsource-repository head\n  type:     git\n  location: git://github.com/haskell/network.git\n";
    }