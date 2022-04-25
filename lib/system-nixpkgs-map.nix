# Maps of system pkg references from cabal file to nixpkgs pkgs.
# library name (from `extra-libraries` field of `.cabal` file) -> nixpkgs package(s) mapping.
# This mapping is used to construct the `libs` component attribute.
# See ../docs/tutorials/pkg-map.md
pkgs:

with pkgs;

let
  stdcplusplus = if pkgs.stdenv.hostPlatform.isWindows then [
    pkgs.windows.mcfgthreads
    (pkgs.evalPackages.runCommand "gcc-only" { nativeBuildInputs = [ pkgs.evalPackages.xorg.lndir ]; } ''
      mkdir $out
      lndir ${pkgs.buildPackages.gcc.cc} $out
    '')
  ] else [];
in
# -- linux
{ crypto = [ openssl ];
  "c++" = []; # no libc++
  "stdc++" = stdcplusplus;
  "stdc++-6" = stdcplusplus;
  ssl = [ openssl ];
  z = [ zlib ];
  pcap = [ libpcap ];
  pthread = null; # available by default
  GL = [ libGL ];
  GLU = [ libGLU ];
  alut = [ freealut ];
  X11 = with xorg; [ libX11 ];
  Xrandr = [ xorg.libXrandr ];
  Xrender = [ xorg.libXrender ];
  Xss = [ xorg.libXScrnSaver ];
  Xext = [ xorg.libXext ];
  Xi = [ xorg.libXi ];
  Xxf86vm = [ xorg.libXxf86vm ];
  Xcursor = [ xorg.libXcursor ];
  Xinerama = [ xorg.libXinerama ];
  mysqlclient = [ mysql ];
  Imlib2 = [ imlib2 ];
  asound = [ alsaLib ];
  ffi = null;
  bz2 = [ bzip2 ];
  util = [ utillinux ];
  magic = [ file ];
  pq = [ postgresql ];
  iconv = [ libiconv ];
  lapack = [ liblapack ];
  boost_atomic = [ boost ];
  boost_chrono = [ boost ];
  boost_container = [ boost ];
  boost_context = [ boost ];
  boost_contract = [ boost ];
  boost_coroutine = [ boost ];
  boost_date_time = [ boost ];
  boost_fiber = [ boost ];
  boost_filesystem = [ boost ];
  boost_graph = [ boost ];
  boost_iostreams = [ boost ];
  boost_locale = [ boost ];
  boost_log_setup = [ boost ];
  boost_log = [ boost ];
  boost_math_c99f = [ boost ];
  boost_math_c99l = [ boost ];
  boost_math_c99 = [ boost ];
  boost_math_tr1f = [ boost ];
  boost_math_tr1l = [ boost ];
  boost_math_tr1 = [ boost ];
  boost_prg_exec_monitor = [ boost ];
  boost_program_options = [ boost ];
  boost_random = [ boost ];
  boost_regex = [ boost ];
  boost_serialization = [ boost ];
  boost_signals = [ boost ];
  boost_stacktrace_addr2line = [ boost ];
  boost_stacktrace_basic = [ boost ];
  boost_stacktrace_noop = [ boost ];
  boost_system = [ boost ];
  boost_thread = [ boost ];
  boost_timer = [ boost ];
  boost_type_erasure = [ boost ];
  boost_unit_test_framework = [ boost ];
  boost_wave = [ boost ];
  boost_wserialization = [ boost ];
  tensorflow = [ libtensorflow ];
  # odbc package requires unixODBC packages to be installed in order to successfully
  # compile C sources (https://github.com/fpco/odbc/blob/master/cbits/odbc.c)
  odbc = [ unixODBC ];
  opencv = [ opencv3 ];
  icuuc = [ icu ];
  icui18n = [ icu ];
  icu-i18n = [ icu ];
  icudata = [ icu ];
  vulkan = [ vulkan-loader ];
  sodium = [ libsodium ];
  gfortran = [ gfortran.cc.lib ];
  ssh2 = [ libssh2 ];
  gpiod = [ libgpiod ];
}
# -- windows
// { advapi32 = null; gdi32 = null; imm32 = null; msimg32 = null;
     shell32 = null; shfolder = null; shlwapi = null; user32 = null;
     ole32 = null; rpcrt4 = null;
     winmm = null; userenv = null;
     kernel32 = null; ws2_32 = null;
     # this should be bundled with gcc.
     # if it's not we have more severe
     # issues anyway.
     gcc_s_seh-1 = null;
     gcc_s = null;
     ssl32 = null; eay32 = [ openssl ];
     iphlpapi = null; # IP Help API
     msvcrt = null; # this is the libc
     Crypt32 = null;
     mswsock = null;
   }
# -- os x
# NB: these map almost 1:1 to the framework names
// darwin.apple_sdk.frameworks
