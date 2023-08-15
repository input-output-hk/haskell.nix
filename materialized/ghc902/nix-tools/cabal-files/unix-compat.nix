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
    flags = { old-time = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "unix-compat"; version = "0.5.4"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Jacob Stanley <jacob@stanley.io>";
      author = "Björn Bringert, Duncan Coutts, Jacob Stanley, Bryan O'Sullivan";
      homepage = "http://github.com/jacobstanley/unix-compat";
      url = "";
      synopsis = "Portable POSIX-compatibility layer.";
      description = "This package provides portable implementations of parts\nof the unix package. This package re-exports the unix\npackage when available. When it isn't available,\nportable implementations are used.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          ] ++ (if system.isWindows
          then [
            (hsPkgs."Win32" or (errorHandler.buildDepError "Win32"))
            ] ++ (if flags.old-time
            then [
              (hsPkgs."old-time" or (errorHandler.buildDepError "old-time"))
              ] ++ [
              (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
              ]
            else [
              (hsPkgs."time" or (errorHandler.buildDepError "time"))
              (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
              ])
          else [ (hsPkgs."unix" or (errorHandler.buildDepError "unix")) ]);
        libs = (pkgs.lib).optional (system.isWindows) (pkgs."msvcrt" or (errorHandler.sysDepError "msvcrt"));
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/unix-compat-0.5.4.tar.gz";
      sha256 = "8224579d6e9acea7ecbd7af21f191758a11c77a2c5b2fc61f1079ac004a4a4b1";
      });
    }) // {
    package-description-override = "name:           unix-compat\r\nversion:        0.5.4\r\nx-revision: 2\r\nsynopsis:       Portable POSIX-compatibility layer.\r\ndescription:    This package provides portable implementations of parts\r\n                of the unix package. This package re-exports the unix\r\n                package when available. When it isn't available,\r\n                portable implementations are used.\r\n\r\nhomepage:       http://github.com/jacobstanley/unix-compat\r\nlicense:        BSD3\r\nlicense-file:   LICENSE\r\nauthor:         Björn Bringert, Duncan Coutts, Jacob Stanley, Bryan O'Sullivan\r\nmaintainer:     Jacob Stanley <jacob@stanley.io>\r\ncategory:       System\r\nbuild-type:     Simple\r\ncabal-version:  >= 1.10\r\n\r\nsource-repository head\r\n  type:     git\r\n  location: git://github.com/jacobstanley/unix-compat.git\r\n\r\nflag old-time\r\n  description: build against old-time package\r\n  default: False\r\n\r\nLibrary\r\n  default-language: Haskell2010\r\n  hs-source-dirs: src\r\n  ghc-options: -Wall\r\n  build-depends: base == 4.*\r\n\r\n  exposed-modules:\r\n    System.PosixCompat\r\n    System.PosixCompat.Extensions\r\n    System.PosixCompat.Files\r\n    System.PosixCompat.Temp\r\n    System.PosixCompat.Time\r\n    System.PosixCompat.Types\r\n    System.PosixCompat.Unistd\r\n    System.PosixCompat.User\r\n\r\n  if os(windows)\r\n    c-sources:\r\n      cbits/HsUname.c\r\n      cbits/mktemp.c\r\n\r\n    extra-libraries: msvcrt\r\n    build-depends: Win32 >= 2.5.0.0\r\n\r\n    if flag(old-time)\r\n      build-depends: old-time >= 1.0.0.0 && < 1.2.0.0\r\n      cpp-options: -DOLD_TIME\r\n\r\n      if impl(ghc < 7)\r\n        build-depends: directory == 1.0.*\r\n        cpp-options: -DDIRECTORY_1_0\r\n      else\r\n        build-depends: directory == 1.1.*\r\n    else\r\n      build-depends: time >= 1.0 && < 1.13\r\n      build-depends: directory >= 1.2 && < 1.4\r\n\r\n    other-modules:\r\n      System.PosixCompat.Internal.Time\r\n\r\n  else\r\n    build-depends: unix >= 2.6 && < 2.8\r\n    include-dirs: include\r\n    includes: HsUnixCompat.h\r\n    install-includes: HsUnixCompat.h\r\n    c-sources: cbits/HsUnixCompat.c\r\n    if os(solaris)\r\n      cc-options: -DSOLARIS\r\n";
    }