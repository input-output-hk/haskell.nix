# Maps of system pkg refenreces from
# cabal file to nixpkgs pkgs.
pkgs:
# fetchgit should always come from the buildPackages
# if it comes from the targetPackages we won't even
# be able to execute it.
   { fetchgit = pkgs.buildPackages.fetchgit; }
# haskell lib -> nix lib mapping
# -- linux
// { crypto = pkgs.openssl;
     "c++" = null; # no libc++
     "stdc++" = null; "stdc++-6" = null;
     ssl = pkgs.openssl;
     z = pkgs.zlib;
     pthread = null; # available by default
     GL = pkgs.libGL;
     GLU = pkgs.libGLU;
     alut = pkgs.freealut;
     X11 = pkgs.xorg.libX11;
     Xrandr = pkgs.xorg.libXrandr;
     Xext = pkgs.xorg.libXext;
     Xi = pkgs.xorg.libXi;
     Xxf86vm = pkgs.xorg.libXxf86vm;
     Xcursor = pkgs.xorg.libXcursor;
     Xinerama = pkgs.xorg.libXinerama;
     mysqlclient = pkgs.mysql;
     Imlib2 = pkgs.imlib2;
     asound = pkgs.alsaLib;
     ffi = null;
     bz2 = pkgs.bzip2;
     util = pkgs.utillinux;
   }
# -- windows
// { advapi32 = null; gdi32 = null; imm32 = null; msimg32 = null;
     shell32 = null; shfolder = null; shlwapi = null; user32 = null;
     winmm = null;
     kernel32 = null; ws2_32 = null;
     # this should be bundled with gcc.
     # if it's not we have more severe
     # issues anyway.
     gcc_s_seh-1 = null;
     ssl32 = null; eay32 = pkgs.openssl;
     iphlpapi = null; # IP Help API
     msvcrt = null; # this is the libc
     Crypt32 = null;
   }
# -- os x
# NB: these map almost 1:1 to the famework names
// pkgs.darwin.apple_sdk.frameworks
