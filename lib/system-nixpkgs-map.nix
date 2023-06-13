# Maps of system pkg references from cabal file to nixpkgs pkgs.
# library name (from `extra-libraries` field of `.cabal` file) -> nixpkgs package(s) mapping.
# This mapping is used to construct the `libs` component attribute.
# See ../docs/tutorials/pkg-map.md
pkgs:

with pkgs;

let
  # On windows systems we need these to be propagatedBuildInputs so that the DLLs will be found.
  gcclibs = if pkgs.stdenv.hostPlatform.isWindows then [
    pkgs.windows.mcfgthreads
    # If we just use `pkgs.buildPackages.gcc.cc` here it breaks the `th-dlls` test. TODO figure out why exactly.
    (pkgs.buildPackages.runCommand "gcc-only" { nativeBuildInputs = [ pkgs.buildPackages.xorg.lndir ]; } ''
      mkdir $out
      lndir ${pkgs.buildPackages.gcc.cc} $out
    '')
  ] else [];
in
# -- linux
{ crypto = [ openssl ];
  "c++" = [ libcxx ];
  "c++abi" = [ libcxxabi ];
  system-cxx-std-lib = [];
  "stdc++" = gcclibs;
  "stdc++-6" = gcclibs;
  gcc_s_seh-1 = gcclibs;
  gcc_s = gcclibs;
  gcc = gcclibs;
  ssl = [ openssl ];
  z = [ zlib ];
  m = []; # Included with ghc
  pcap = [ libpcap ];
  pthread = null; # available by default
  GL = [ libGL ];
  GLEW = [ glew ];
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
  pgcommon = [ postgresql ];
  pgport = [ postgresql] ;
  pq = [ postgresql ];
  libpq = [ postgresql ];
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
  png = [ libpng ];
  jpeg = [ libjpeg ];
  freenect_sync = [ freenect ];
  FLAC = [ flac ];
  mp3lame = [ lame ];
  tag_c = [ taglib ];
  jwt = [ libjwt ];
  GeoIP = [ geoip ];
  pulse-simple = [ libpulseaudio ];
  oath = [ liboauth ];
}
# -- windows
// { advapi32 = null; gdi32 = null; imm32 = null; msimg32 = null;
     shell32 = null; shfolder = null; shlwapi = null; user32 = null;
     ole32 = null; rpcrt4 = null;
     winmm = null; userenv = null;
     kernel32 = null; ws2_32 = null;
     opengl32 = null; glu32 = null;
     # this should be bundled with gcc.
     # if it's not we have more severe
     # issues anyway.
     ssl32 = null; eay32 = [ openssl ];
     iphlpapi = null; # IP Help API
     msvcrt = null; # this is the libc
     Crypt32 = null;
     mswsock = null;
     bcrypt = null;
   }
# -- os x
# NB: these map almost 1:1 to the framework names
// darwin.apple_sdk.frameworks
