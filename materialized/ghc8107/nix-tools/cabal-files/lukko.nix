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
    flags = { ofd-locking = true; };
    package = {
      specVersion = "2.2";
      identifier = { name = "lukko"; version = "0.1.1.3"; };
      license = "GPL-2.0-or-later AND BSD-3-Clause";
      copyright = "";
      maintainer = "Oleg Grenrus <oleg.grenrus@iki.fi>";
      author = "";
      homepage = "";
      url = "";
      synopsis = "File locking";
      description = "This package provides access to platform dependent file locking APIs:\n\n* <https://www.gnu.org/software/libc/manual/html_node/Open-File-Description-Locks.html Open file descriptor locking> on Linux (\"Lukko.OFD\")\n* BSD-style @flock(2)@ locks on UNIX platforms (\"Lukko.FLock\")\n* Windows locking via <https://docs.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-lockfilee LockFileEx> (\"Lukko.Windows\")\n* No-op locking, which throws exceptions (\"Lukko.NoOp\")\n* \"Lukko\" module exports the best option for the target platform with uniform API.\n\nThere are alternative file locking packages:\n\n* \"GHC.IO.Handle.Lock\" in @base >= 4.10@ is good enough for most use cases.\nHowever, uses only 'Handle's so these locks cannot be used for intra-process locking.\n(You should use e.g. 'MVar' in addition).\n\n* <https://hackage.haskell.org/package/filelock filelock> doesn't support OFD locking.\n\n/Lukko/ means lock in Finnish.\n\nSubmodules \"Lukko.OFD\", \"Lukko.Windows\" etc are available based on following conditions.\n\n@\nif os(windows)\n\\  cpp-options: -DHAS_WINDOWS_LOCK\n\nelif (os(linux) && flag(ofd-locking))\n\\  cpp-options: -DHAS_OFD_LOCKING\n\\  cpp-options: -DHAS_FLOCK\n\nelif !(os(solaris) || os(aix))\n\\  cpp-options: -DHAS_FLOCK\n@\n\n\"Lukko.FLock\" is available on not (Windows or Solaris or AIX).\n\"Lukko.NoOp\" is always available.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [ (hsPkgs."base" or (errorHandler.buildDepError "base")) ];
        build-tools = [
          (hsPkgs.buildPackages.hsc2hs.components.exes.hsc2hs or (pkgs.buildPackages.hsc2hs or (errorHandler.buildToolDepError "hsc2hs:hsc2hs")))
          ];
        buildable = true;
        };
      tests = {
        "test-thread" = {
          depends = [
            (hsPkgs."async" or (errorHandler.buildDepError "async"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."lukko" or (errorHandler.buildDepError "lukko"))
            (hsPkgs."singleton-bool" or (errorHandler.buildDepError "singleton-bool"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-expected-failure" or (errorHandler.buildDepError "tasty-expected-failure"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."temporary" or (errorHandler.buildDepError "temporary"))
            ] ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "7.8")) (hsPkgs."tagged" or (errorHandler.buildDepError "tagged"));
          buildable = true;
          };
        "test-process" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."lukko" or (errorHandler.buildDepError "lukko"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/lukko-0.1.1.3.tar.gz";
      sha256 = "a80efb60cfa3dae18682c01980d76d5f7e413e191cd186992e1bf7388d48ab1f";
      });
    }