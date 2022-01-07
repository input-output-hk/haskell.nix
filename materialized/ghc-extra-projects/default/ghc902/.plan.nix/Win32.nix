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
      specVersion = "2.0";
      identifier = { name = "Win32"; version = "2.12.0.1"; };
      license = "BSD-3-Clause";
      copyright = "Alastair Reid, 1999-2003; shelarcy, 2012-2013; Tamar Christina, 2016-2020";
      maintainer = "Haskell Libraries <libraries@haskell.org>";
      author = "Alastair Reid, shelarcy, Tamar Christina";
      homepage = "https://github.com/haskell/win32";
      url = "";
      synopsis = "A binding to Windows Win32 API.";
      description = "This library contains direct bindings to the Windows Win32 APIs for Haskell.";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" ];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [
        "include/diatemp.h"
        "include/dumpBMP.h"
        "include/ellipse.h"
        "include/errors.h"
        "include/Win32Aux.h"
        "include/win32debug.h"
        "include/alignment.h"
        "changelog.md"
        ];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          ] ++ (pkgs.lib).optional (!system.isWindows) (hsPkgs."unbuildable" or (errorHandler.buildDepError "unbuildable"));
        libs = [
          (pkgs."user32" or (errorHandler.sysDepError "user32"))
          (pkgs."gdi32" or (errorHandler.sysDepError "gdi32"))
          (pkgs."winmm" or (errorHandler.sysDepError "winmm"))
          (pkgs."advapi32" or (errorHandler.sysDepError "advapi32"))
          (pkgs."shell32" or (errorHandler.sysDepError "shell32"))
          (pkgs."shfolder" or (errorHandler.sysDepError "shfolder"))
          (pkgs."shlwapi" or (errorHandler.sysDepError "shlwapi"))
          (pkgs."msimg32" or (errorHandler.sysDepError "msimg32"))
          (pkgs."imm32" or (errorHandler.sysDepError "imm32"))
          ];
        build-tools = [
          (hsPkgs.buildPackages.hsc2hs.components.exes.hsc2hs or (pkgs.buildPackages.hsc2hs or (errorHandler.buildToolDepError "hsc2hs:hsc2hs")))
          ];
        buildable = if !system.isWindows then false else true;
        modules = [
          "Graphics/Win32/GDI"
          "Graphics/Win32/GDI/Bitmap"
          "Graphics/Win32/GDI/Brush"
          "Graphics/Win32/GDI/Clip"
          "Graphics/Win32/GDI/Font"
          "Graphics/Win32/GDI/Graphics2D"
          "Graphics/Win32/GDI/HDC"
          "Graphics/Win32/GDI/Palette"
          "Graphics/Win32/GDI/Path"
          "Graphics/Win32/GDI/Pen"
          "Graphics/Win32/GDI/Region"
          "Graphics/Win32/GDI/Types"
          "Graphics/Win32"
          "Graphics/Win32/Control"
          "Graphics/Win32/Dialogue"
          "Graphics/Win32/Icon"
          "Graphics/Win32/Key"
          "Graphics/Win32/Menu"
          "Graphics/Win32/Message"
          "Graphics/Win32/Misc"
          "Graphics/Win32/Resource"
          "Graphics/Win32/Window"
          "Graphics/Win32/LayeredWindow"
          "Graphics/Win32/GDI/AlphaBlend"
          "Graphics/Win32/Window/AnimateWindow"
          "Graphics/Win32/Window/HotKey"
          "Graphics/Win32/Window/IMM"
          "Graphics/Win32/Window/ForegroundWindow"
          "Graphics/Win32/Window/PostMessage"
          "Media/Win32"
          "System/Win32"
          "System/Win32/DebugApi"
          "System/Win32/DLL"
          "System/Win32/Event"
          "System/Win32/File"
          "System/Win32/FileMapping"
          "System/Win32/Info"
          "System/Win32/Path"
          "System/Win32/Mem"
          "System/Win32/MinTTY"
          "System/Win32/NLS"
          "System/Win32/Process"
          "System/Win32/Registry"
          "System/Win32/SimpleMAPI"
          "System/Win32/Time"
          "System/Win32/Console"
          "System/Win32/Security"
          "System/Win32/Types"
          "System/Win32/Shell"
          "System/Win32/Automation"
          "System/Win32/Automation/Input"
          "System/Win32/Automation/Input/Key"
          "System/Win32/Automation/Input/Mouse"
          "System/Win32/Console/CtrlHandler"
          "System/Win32/Console/HWND"
          "System/Win32/Console/Title"
          "System/Win32/Encoding"
          "System/Win32/Exception/Unsupported"
          "System/Win32/HardLink"
          "System/Win32/Info/Computer"
          "System/Win32/Info/Version"
          "System/Win32/String"
          "System/Win32/SymbolicLink"
          "System/Win32/Thread"
          "System/Win32/Utils"
          "System/Win32/Word"
          ];
        cSources = [
          "cbits/HsGDI.c"
          "cbits/HsWin32.c"
          "cbits/WndProc.c"
          "cbits/diatemp.c"
          "cbits/dumpBMP.c"
          "cbits/ellipse.c"
          "cbits/errors.c"
          "cbits/alphablend.c"
          ];
        includeDirs = [ "include" ];
        includes = [
          "alphablend.h"
          "diatemp.h"
          "dumpBMP.h"
          "ellipse.h"
          "errors.h"
          "HsGDI.h"
          "HsWin32.h"
          "Win32Aux.h"
          "win32debug.h"
          "windows_cconv.h"
          "WndProc.h"
          "alignment.h"
          ];
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../libraries/Win32; }