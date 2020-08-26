# Maps of system pkg refenreces from
# cabal file to nixpkgs pkgs.
# See ../docs/dev/pkg-map.md
pkgs:
# fetchgit should always come from the evalPackages
# if it comes from the targetPackages we won't even
# be able to execute it.
   { fetchgit = pkgs.evalPackages.fetchgit; }
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
     magic = pkgs.file;
     pq = pkgs.postgresql;
     iconv = pkgs.libiconv;
     lapack = pkgs.liblapack;
     boost_atomic = pkgs.boost;
     boost_chrono = pkgs.boost;
     boost_container = pkgs.boost;
     boost_context = pkgs.boost;
     boost_contract = pkgs.boost;
     boost_coroutine = pkgs.boost;
     boost_date_time = pkgs.boost;
     boost_fiber = pkgs.boost;
     boost_filesystem = pkgs.boost;
     boost_graph = pkgs.boost;
     boost_iostreams = pkgs.boost;
     boost_locale = pkgs.boost;
     boost_log_setup = pkgs.boost;
     boost_log = pkgs.boost;
     boost_math_c99f = pkgs.boost;
     boost_math_c99l = pkgs.boost;
     boost_math_c99 = pkgs.boost;
     boost_math_tr1f = pkgs.boost;
     boost_math_tr1l = pkgs.boost;
     boost_math_tr1 = pkgs.boost;
     boost_prg_exec_monitor = pkgs.boost;
     boost_program_options = pkgs.boost;
     boost_random = pkgs.boost;
     boost_regex = pkgs.boost;
     boost_serialization = pkgs.boost;
     boost_signals = pkgs.boost;
     boost_stacktrace_addr2line = pkgs.boost;
     boost_stacktrace_basic = pkgs.boost;
     boost_stacktrace_noop = pkgs.boost;
     boost_system = pkgs.boost;
     boost_thread = pkgs.boost;
     boost_timer = pkgs.boost;
     boost_type_erasure = pkgs.boost;
     boost_unit_test_framework = pkgs.boost;
     boost_wave = pkgs.boost;
     boost_wserialization = pkgs.boost;
     tensorflow = pkgs.libtensorflow;
     opencv = pkgs.opencv3;
     icuuc = pkgs.icu;
     icui18n = pkgs.icu;
     icudata = pkgs.icu;
     vulkan = pkgs.vulkan-loader;
     sodium = pkgs.libsodium;
     gfortran = pkgs.gfortran.cc.lib;
   }
# -- windows
// { advapi32 = null; gdi32 = null; imm32 = null; msimg32 = null;
     shell32 = null; shfolder = null; shlwapi = null; user32 = null;
     winmm = null; userenv = null;
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
