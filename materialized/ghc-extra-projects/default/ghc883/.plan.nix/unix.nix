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
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = { name = "unix"; version = "2.7.2.2"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "libraries@haskell.org";
      author = "";
      homepage = "https://github.com/haskell/unix";
      url = "";
      synopsis = "POSIX functionality";
      description = "This package gives you access to the set of operating system\nservices standardised by\n<http://pubs.opengroup.org/onlinepubs/9699919799/ POSIX.1-2008>\n(or the IEEE Portable Operating System Interface for Computing\nEnvironments - IEEE Std. 1003.1).\n\nThe package is not supported under Windows.";
      buildType = "Configure";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" ];
      dataDir = "";
      dataFiles = [];
      extraSrcFiles = [
        "changelog.md"
        "config.guess"
        "config.sub"
        "configure"
        "configure.ac"
        "include/HsUnix.h"
        "include/HsUnixConfig.h.in"
        "install-sh"
        "unix.buildinfo.in"
        ];
      extraTmpFiles = [
        "autom4te.cache"
        "config.log"
        "config.status"
        "include/HsUnixConfig.h"
        "unix.buildinfo"
        ];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          ] ++ (pkgs.lib).optional (system.isWindows) (hsPkgs."unbuildable" or (errorHandler.buildDepError "unbuildable"));
        buildable = if system.isWindows then false else true;
        modules = [
          "System/Posix/Directory/Common"
          "System/Posix/DynamicLinker/Common"
          "System/Posix/Files/Common"
          "System/Posix/IO/Common"
          "System/Posix/Process/Common"
          "System/Posix/Terminal/Common"
          "System/Posix"
          "System/Posix/ByteString"
          "System/Posix/Error"
          "System/Posix/Resource"
          "System/Posix/Time"
          "System/Posix/Unistd"
          "System/Posix/User"
          "System/Posix/Signals"
          "System/Posix/Signals/Exts"
          "System/Posix/Semaphore"
          "System/Posix/SharedMem"
          "System/Posix/ByteString/FilePath"
          "System/Posix/Directory"
          "System/Posix/Directory/ByteString"
          "System/Posix/DynamicLinker/Module"
          "System/Posix/DynamicLinker/Module/ByteString"
          "System/Posix/DynamicLinker/Prim"
          "System/Posix/DynamicLinker/ByteString"
          "System/Posix/DynamicLinker"
          "System/Posix/Files"
          "System/Posix/Files/ByteString"
          "System/Posix/IO"
          "System/Posix/IO/ByteString"
          "System/Posix/Env"
          "System/Posix/Env/ByteString"
          "System/Posix/Fcntl"
          "System/Posix/Process"
          "System/Posix/Process/Internals"
          "System/Posix/Process/ByteString"
          "System/Posix/Temp"
          "System/Posix/Temp/ByteString"
          "System/Posix/Terminal"
          "System/Posix/Terminal/ByteString"
          ];
        cSources = [ "cbits/HsUnix.c" "cbits/execvpe.c" ];
        includeDirs = [ "include" ];
        includes = [ "HsUnix.h" "execvpe.h" ];
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../libraries/unix; }