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
      specVersion = "1.10";
      identifier = { name = "Win32"; version = "2.8.3.0"; };
      license = "BSD-3-Clause";
      copyright = "Alastair Reid, 1999-2003; shelarcy, 2012-2013; Tamar Christina, 2016-2018";
      maintainer = "Haskell Libraries <libraries@haskell.org>";
      author = "Alastair Reid, shelarcy, Tamar Christina";
      homepage = "https://github.com/haskell/win32";
      url = "";
      synopsis = "A binding to Windows Win32 API.";
      description = "This library contains direct bindings to the Windows Win32 APIs for Haskell.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
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
        buildable = if !system.isWindows then false else true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/Win32-2.8.3.0.tar.gz";
      sha256 = "593fbbfef98546a224e4652aa0423b0374da8c109cd4e48f16e16b17c21f5c63";
      });
    }) // {
    package-description-override = "name:\t\tWin32\nversion:\t2.8.3.0\nlicense:\tBSD3\nlicense-file:\tLICENSE\nauthor:\t\tAlastair Reid, shelarcy, Tamar Christina\ncopyright:\tAlastair Reid, 1999-2003; shelarcy, 2012-2013; Tamar Christina, 2016-2018\nmaintainer:\tHaskell Libraries <libraries@haskell.org>\nbug-reports:    https://github.com/haskell/win32/issues\nhomepage:       https://github.com/haskell/win32\ncategory:\tSystem, Graphics\nsynopsis:\tA binding to Windows Win32 API.\ndescription:\tThis library contains direct bindings to the Windows Win32 APIs for Haskell.\nbuild-type:     Simple\ncabal-version:  >=1.10\nextra-source-files:\n        include/diatemp.h include/dumpBMP.h include/ellipse.h include/errors.h\n        include/Win32Aux.h include/win32debug.h include/alignment.h\n        changelog.md\n\nLibrary\n    default-language: Haskell2010\n    default-extensions: ForeignFunctionInterface, CPP\n    if impl(ghc >= 7.1)\n        default-extensions: NondecreasingIndentation\n\n    if !os(windows)\n        -- This package requires Windows to build\n        build-depends: unbuildable<0\n        buildable: False\n\n    build-depends:\tbase >= 4.5 && < 5, bytestring, filepath\n    ghc-options:    -Wall -fno-warn-name-shadowing\n    cc-options:     -fno-strict-aliasing\n    exposed-modules:\n        Graphics.Win32.GDI\n        Graphics.Win32.GDI.Bitmap\n        Graphics.Win32.GDI.Brush\n        Graphics.Win32.GDI.Clip\n        Graphics.Win32.GDI.Font\n        Graphics.Win32.GDI.Graphics2D\n        Graphics.Win32.GDI.HDC\n        Graphics.Win32.GDI.Palette\n        Graphics.Win32.GDI.Path\n        Graphics.Win32.GDI.Pen\n        Graphics.Win32.GDI.Region\n        Graphics.Win32.GDI.Types\n        Graphics.Win32\n        Graphics.Win32.Control\n        Graphics.Win32.Dialogue\n        Graphics.Win32.Icon\n        Graphics.Win32.Key\n        Graphics.Win32.Menu\n        Graphics.Win32.Message\n        Graphics.Win32.Misc\n        Graphics.Win32.Resource\n        Graphics.Win32.Window\n        Graphics.Win32.LayeredWindow\n        Graphics.Win32.GDI.AlphaBlend\n        Graphics.Win32.Window.AnimateWindow\n        Graphics.Win32.Window.HotKey\n        Graphics.Win32.Window.IMM\n        Graphics.Win32.Window.ForegroundWindow\n        Graphics.Win32.Window.PostMessage\n\n        Media.Win32\n\n        System.Win32\n        System.Win32.DebugApi\n        System.Win32.DLL\n        System.Win32.File\n        System.Win32.FileMapping\n        System.Win32.Info\n        System.Win32.Path\n        System.Win32.Mem\n        System.Win32.MinTTY\n        System.Win32.NLS\n        System.Win32.Process\n        System.Win32.Registry\n        System.Win32.SimpleMAPI\n        System.Win32.Time\n        System.Win32.Console\n        System.Win32.Security\n        System.Win32.Types\n        System.Win32.Shell\n        System.Win32.Automation\n        System.Win32.Automation.Input\n        System.Win32.Automation.Input.Key\n        System.Win32.Automation.Input.Mouse\n        System.Win32.Console.CtrlHandler\n        System.Win32.Console.HWND\n        System.Win32.Console.Title\n        System.Win32.Encoding\n        System.Win32.Exception.Unsupported\n        System.Win32.HardLink\n        System.Win32.Info.Computer\n        System.Win32.Info.Version\n        System.Win32.String\n        System.Win32.SymbolicLink\n        System.Win32.Thread\n        System.Win32.Utils\n        System.Win32.Word\n\n    extra-libraries:\n        \"user32\", \"gdi32\", \"winmm\", \"advapi32\", \"shell32\", \"shfolder\", \"shlwapi\", \"msimg32\", \"imm32\"\n    ghc-options:      -Wall\n    include-dirs:     include\n    includes:         \"alphablend.h\", \"diatemp.h\", \"dumpBMP.h\", \"ellipse.h\", \"errors.h\", \"HsGDI.h\", \"HsWin32.h\", \"Win32Aux.h\", \"win32debug.h\", \"windows_cconv.h\", \"WndProc.h\", \"alignment.h\"\n    install-includes: \"HsWin32.h\", \"HsGDI.h\", \"WndProc.h\", \"windows_cconv.h\", \"alphablend.h\", \"winternl_compat.h\", \"winuser_compat.h\", \"winreg_compat.h\", \"tlhelp32_compat.h\"\n    c-sources:\n        cbits/HsGDI.c\n        cbits/HsWin32.c\n        cbits/WndProc.c\n        cbits/diatemp.c\n        cbits/dumpBMP.c\n        cbits/ellipse.c\n        cbits/errors.c\n        cbits/alphablend.c\n    cc-options: -Wall\n\nsource-repository head\n    type:     git\n    location: git://github.com/haskell/win32\n";
    }