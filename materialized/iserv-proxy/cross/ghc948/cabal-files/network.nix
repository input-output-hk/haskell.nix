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
    flags = { devel = false; };
    package = {
      specVersion = "1.18";
      identifier = { name = "network"; version = "3.1.4.0"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Kazu Yamamoto, Evan Borden";
      author = "";
      homepage = "https://github.com/haskell/network";
      url = "";
      synopsis = "Low-level networking interface";
      description = "This package provides a low-level networking interface.\n\n=== High-Level Packages\nOther packages provide higher level interfaces:\n\n* connection\n* hookup\n* network-simple\n\n=== Extended Packages\n@network@ seeks to provide a cross-platform core for networking. As such some\nAPIs live in extended libraries. Packages in the @network@ ecosystem are\noften prefixed with @network-@.\n\n==== @network-bsd@\nIn @network-3.0.0.0@ the @Network.BSD@ module was split off into its own\npackage, @network-bsd-3.0.0.0@.\n\n==== @network-uri@\nIn @network-2.6@ the @Network.URI@ module was split off into its own package,\n@network-uri-2.6@. If you're using the @Network.URI@ module you can\nautomatically get it from the right package by adding this to your @.cabal@\nfile:\n\n> library\n>   build-depends: network-uri-flag";
      buildType = "Configure";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          ] ++ (pkgs.lib).optional (system.isWindows) (hsPkgs."temporary" or (errorHandler.buildDepError "temporary"));
        libs = (pkgs.lib).optionals (system.isSolaris) [
          (pkgs."nsl" or (errorHandler.sysDepError "nsl"))
          (pkgs."socket" or (errorHandler.sysDepError "socket"))
          ] ++ (pkgs.lib).optionals (system.isWindows) [
          (pkgs."ws2_32" or (errorHandler.sysDepError "ws2_32"))
          (pkgs."iphlpapi" or (errorHandler.sysDepError "iphlpapi"))
          (pkgs."mswsock" or (errorHandler.sysDepError "mswsock"))
          ];
        build-tools = [
          (hsPkgs.buildPackages.hsc2hs.components.exes.hsc2hs or (pkgs.buildPackages.hsc2hs or (errorHandler.buildToolDepError "hsc2hs:hsc2hs")))
          ];
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
            (hsPkgs."temporary" or (errorHandler.buildDepError "temporary"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
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
      url = "http://hackage.haskell.org/package/network-3.1.4.0.tar.gz";
      sha256 = "b452a2afac95d9207357eb3820c719c7c7d27871ef4b6ed7bfcd03a036b9158e";
      });
    }) // {
    package-description-override = "cabal-version:      1.18\nname:               network\nversion:            3.1.4.0\nx-revision:         1\nlicense:            BSD3\nlicense-file:       LICENSE\nmaintainer:         Kazu Yamamoto, Evan Borden\n\ntested-with:\n    GHC == 9.6.1\n    GHC == 9.4.4\n    GHC == 9.2.7\n    GHC == 9.0.2\n    GHC == 8.10.7\n    GHC == 8.8.4\n    GHC == 8.6.5\n    GHC == 8.4.4\n    GHC == 8.2.2\n    GHC == 8.0.2\n\nhomepage:           https://github.com/haskell/network\nbug-reports:        https://github.com/haskell/network/issues\nsynopsis:           Low-level networking interface\ndescription:\n    This package provides a low-level networking interface.\n    .\n    === High-Level Packages\n    Other packages provide higher level interfaces:\n    .\n    * connection\n    * hookup\n    * network-simple\n    .\n    === Extended Packages\n    @network@ seeks to provide a cross-platform core for networking. As such some\n    APIs live in extended libraries. Packages in the @network@ ecosystem are\n    often prefixed with @network-@.\n    .\n    ==== @network-bsd@\n    In @network-3.0.0.0@ the @Network.BSD@ module was split off into its own\n    package, @network-bsd-3.0.0.0@.\n    .\n    ==== @network-uri@\n    In @network-2.6@ the @Network.URI@ module was split off into its own package,\n    @network-uri-2.6@. If you're using the @Network.URI@ module you can\n    automatically get it from the right package by adding this to your @.cabal@\n    file:\n    .\n    > library\n    >   build-depends: network-uri-flag\n\ncategory:           Network\nbuild-type:         Configure\n\nextra-doc-files:\n    README.md\n    CHANGELOG.md\n\nextra-source-files:\n    examples/*.hs\n    tests/*.hs\n    config.guess\n    config.sub\n    install-sh\n    configure.ac\n    configure\n    include/HsNetworkConfig.h.in\n    include/HsNet.h\n    include/HsNetDef.h\n    include/afunix_compat.h\n    cbits/asyncAccept.c\n    cbits/initWinSock.c\n    cbits/winSockErr.c\n    cbits/cmsg.c\n\nextra-tmp-files:\n    config.log\n    config.status\n    autom4te.cache\n    network.buildinfo\n    include/HsNetworkConfig.h\n\nsource-repository head\n    type:     git\n    location: https://github.com/haskell/network.git\n\nflag devel\n    description: using tests for developers\n    default:     False\n\nlibrary\n    exposed-modules:\n        Network.Socket\n        Network.Socket.Address\n        Network.Socket.ByteString\n        Network.Socket.ByteString.Lazy\n        Network.Socket.Internal\n\n    build-tools:      hsc2hs >=0\n    c-sources:\n        cbits/HsNet.c\n        cbits/cmsg.c\n\n    other-modules:\n        Network.Socket.Buffer\n        Network.Socket.ByteString.IO\n        Network.Socket.ByteString.Internal\n        Network.Socket.Cbits\n        Network.Socket.Fcntl\n        Network.Socket.Flag\n        Network.Socket.Handle\n        Network.Socket.If\n        Network.Socket.Imports\n        Network.Socket.Info\n        Network.Socket.Name\n        Network.Socket.Options\n        Network.Socket.ReadShow\n        Network.Socket.Shutdown\n        Network.Socket.SockAddr\n        Network.Socket.Syscall\n        Network.Socket.Types\n        Network.Socket.Unix\n\n    default-language: Haskell2010\n    include-dirs:     include\n    includes:         HsNet.h HsNetDef.h alignment.h win32defs.h\n    install-includes: HsNet.h HsNetDef.h alignment.h win32defs.h\n    if os(windows)\n        includes:         afunix_compat.h\n        install-includes: afunix_compat.h\n\n    ghc-options:      -Wall -fwarn-tabs\n    build-depends:\n        base >=4.9 && <5,\n        bytestring >=0.10 && <0.13,\n        deepseq,\n        directory\n\n    if !os(windows)\n        other-modules:\n            Network.Socket.ByteString.Lazy.Posix\n            Network.Socket.Posix.Cmsg\n            Network.Socket.Posix.CmsgHdr\n            Network.Socket.Posix.IOVec\n            Network.Socket.Posix.MsgHdr\n\n    if os(solaris)\n        cpp-options:     -D__EXTENSIONS__ -D_XOPEN_SOURCE=500\n        cc-options:      -D__EXTENSIONS__ -D_XOPEN_SOURCE=500\n        extra-libraries:\n            nsl\n            socket\n\n    if os(windows)\n        c-sources:\n            cbits/initWinSock.c\n            cbits/winSockErr.c\n            cbits/asyncAccept.c\n\n        other-modules:\n            Network.Socket.ByteString.Lazy.Windows\n            Network.Socket.Win32.Cmsg\n            Network.Socket.Win32.CmsgHdr\n            Network.Socket.Win32.WSABuf\n            Network.Socket.Win32.MsgHdr\n\n        extra-libraries:\n            ws2_32\n            iphlpapi\n            mswsock\n\n        if impl(ghc >=7.10)\n            cpp-options: -D_WIN32_WINNT=0x0600\n            cc-options:  -D_WIN32_WINNT=0x0600\n\n        build-depends:\n            temporary\n\ntest-suite spec\n    type:             exitcode-stdio-1.0\n    main-is:          Spec.hs\n    build-tools:      hspec-discover >=2.6\n    hs-source-dirs:   tests\n    other-modules:\n        Network.Test.Common\n        Network.SocketSpec\n        Network.Socket.ByteStringSpec\n        Network.Socket.ByteString.LazySpec\n\n    default-language: Haskell2010\n    ghc-options:      -Wall -threaded\n    build-depends:\n        base >=4.9 && <5,\n        bytestring,\n        directory,\n        HUnit,\n        network,\n        temporary,\n        hspec >=2.6,\n        QuickCheck\n\n    if flag(devel)\n        cpp-options: -DDEVELOPMENT\n";
    }