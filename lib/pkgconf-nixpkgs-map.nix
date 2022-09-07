# pkgconfig entries to nixpkgs map
# See ../docs/tutorials/pkg-map.md
pkgs:
  let
    # Only include derivations that exist in the current pkgs.
    # This allows us to use this mapping to be used in allPkgConfigWrapper.
    # See ./overlas
    lookupAttrsIn = x: __mapAttrs (pname: names:
        # The first entry is should be used for the version by allPkgConfigWrapper
        # so we need it to be present.
        if __length names != 0 && x ? ${__head names}
          then
            pkgs.lib.concatMap (
              name: if x ? ${name} then [ x.${name} ] else []) names
          else []);
  in lookupAttrsIn pkgs {
    # Based on https://github.com/NixOS/cabal2nix/blob/11c68fdc79461fb74fa1dfe2217c3709168ad752/src/Distribution/Nixpkgs/Haskell/FromCabal/Name.hs#L23
    "adns"                               = [ "adns" ];
    "alsa"                               = [ "alsaLib" ];
    "alut"                               = [ "freealut" ];
    "appindicator-0.1"                   = [ "libappindicator-gtk2" ];
    "appindicator3-0.1"                  = [ "libappindicator-gtk3" ];
    "asound"                             = [ "alsaLib" ];
    "atk"                                = [ "atk" ];
    "b2"                                 = [ "libb2" ];
    "bdw-gc"                             = [ "boehmgc" ];
    "bz2"                                = [ "bzip2" ];
    "c++"                                = []; # What is that?
    "cairo-1.0"                          = [ "cairo" ];
    "cairo-gobject"                      = [ "cairo" ];
    "cairo-pdf"                          = [ "cairo" ];
    "cairo-ps"                           = [ "cairo" ];
    "cairo-svg"                          = [ "cairo" ];
    "crypt"                              = []; # provided by glibc
    "curses"                             = [ "ncurses" ];
    "dbusmenu-glib-0.4"                  = [ "libdbusmenu" ];
    "dbusmenu-gtk3-0.4"                  = [ "libdbusmenu-gtk3" ]; # do we also need "gtk3"
    "dl"                                 = []; # provided by glibc
    "fftw3"                              = [ "fftw" ];
    "fftw3f"                             = [ "fftwFloat" ];
    "freetype2"                          = [ "freetype" ];
    "gconf"                              = [ "GConf" ];
    "gconf-2.0"                          = [ "GConf" ];
    "gdk-2.0"                            = [ "gtk2" ];
    "gdk-3.0"                            = [ "gtk3" ];
    "gdk-x11-2.0"                        = [ "gdk_x11" ];
    "gdk-x11-3.0"                        = [ "gtk3" ];
    "gio-2.0"                            = [ "glib" ];
    "glib-2.0"                           = [ "glib" ];
    "GL"                                 = [ "libGL" ];
    "GLU"                                = [ "libGLU" "libGL" ];
    "glut"                               = [ "freeglut" "libGLU" "libGL" ];
    "gnome-keyring"                      = [ "gnome-keyring" ];
    "gnome-keyring-1"                    = [ "libgnome-keyring" ];
    "gnome-vfs-2.0"                      = [ "gnome-vfs" ];
    "gnome-vfs-module-2.0"               = [ "gnome-vfs_module" ];
    "gobject-2.0"                        = [ "glib" ];
    "gmodule-2.0"                        = [ "glib" ];
    "gobject-introspection-1.0"          = [ "gobject-introspection" ];
    "graphene-gobject-1.0"               = [ "graphene" ];
    "gstreamer-audio-0.10"               = [ "gst-plugins-base" ];
    "gstreamer-audio-1.0"                = [ "gst-plugins-base" ];
    "gstreamer-base-0.10"                = [ "gst-plugins-base" ];
    "gstreamer-base-1.0"                 = [ "gst-plugins-base" ];
    "gstreamer-controller-0.10"          = [ "gstreamer" ];
    "gstreamer-dataprotocol-0.10"        = [ "gstreamer" ];
    "gstreamer-net-0.10"                 = [ "gst-plugins-base" ];
    "gstreamer-plugins-base-0.10"        = [ "gst-plugins-base" ];
    "gstreamer-video-1.0"                = [ "gst-plugins-base" ];
    "gthread-2.0"                        = [ "glib" ];
    "gtk+-2.0"                           = [ "gtk2" ];
    "gtk+-3.0"                           = [ "gtk3" ];
    "gtk-x11-2.0"                        = [ "gtk_x11" ];
    "gtksourceview-3.0"                  = [ "gtksourceview3" ];
    "harfbuzz-gobject"                   = [ "harfbuzz" ];
    "hidapi-libusb"                      = [ "hidapi" ];
    "icudata"                            = [ "icu" ];
    "icui18n"                            = [ "icu" ];
    "icu-i18n"                           = [ "icu" ];
    "icuuc"                              = [ "icu" ];
    "idn"                                = [ "libidn" ];
    "IL"                                 = [ "libdevil" ];
    "ImageMagick"                        = [ "imagemagick" ];
    "Imlib2"                             = [ "imlib2" ];
    "iw"                                 = [ "wirelesstools" ];
    "jack"                               = [ "libjack2" ];
    "javascriptcoregtk-3.0"              = [ "webkitgtk24x-gtk3" ]; # These are the old APIs, of which 2.4 is the last provider, so map directly to that.
    "javascriptcoregtk-4.0"              = [ "webkitgtk" ];
    "jpeg"                               = [ "libjpeg" ];
    "jvm"                                = [ "jdk" ];
    "lapack"                             = [ "liblapack" ];
    "lber"                               = [ "openldap" ];
    "ldap"                               = [ "openldap" ];
    "libavutil"                          = [ "ffmpeg" ];
    "libcrypto"                          = [ "openssl" ];
    "libfreenect"                        = [ "freenect" ];
    "libgsasl"                           = [ "gsasl" ];
    "liboath"                            = [ "liboauth" ];
    "libpcre"                            = [ "pcre" ];
    "libqrencode"                        = [ "qrencode" ];
    "libR"                               = [ "R" ];
    "librsvg-2.0"                        = [ "librsvg" ];
    "libsecp256k1"                       = [ "secp256k1" ];
    "libsodium"                          = [ "libsodium" ];
    "libsoup-2.4"                        = [ "libsoup" ];
    "libsoup-gnome-2.4"                  = [ "libsoup" ];
    "libssl"                             = [ "openssl" ];
    "libsystemd"                         = [ "systemd" ];
    "libudev"                            = [ "systemd" ];
    "libusb-1.0"                         = [ "libusb1" ];
    "libxml-2.0"                         = [ "libxml2" ];
    "libzip"                             = [ "libzip" ];
    "libzmq"                             = [ "zeromq" ];
    "libzstd"                            = [ "zstd" ];
    "m"                                  = []; # in stdenv
    "magic"                              = [ "file" ];
    "MagickWand"                         = [ "imagemagick" ];
    "mnl"                                = [ "libmnl" ];
    "mpi"                                = [ "openmpi" ];
    "ncursesw"                           = [ "ncurses" ];
    "nix-expr"                           = [ "nix" ];
    "nix-main"                           = [ "nix" ];
    "nix-store"                          = [ "nix" ];
    "netsnmp"                            = [ "net_snmp" ];
    "notify"                             = [ "libnotify" ];
    "odbc"                               = [ "unixODBC" ];
    "openblas"                           = [ "openblasCompat" ];
    "opencv"                             = [ "opencv3" ];
    "panelw"                             = [ "ncurses" ];
    "pangocairo"                         = [ "pango" ];
    "pcap"                               = [ "libpcap" ];
    "pfs-1.2"                            = [ "pfstools" ];
    "png"                                = [ "libpng" ];
    "poppler-glib"                       = [ "poppler" ];
    "poppler-cpp"                        = [ "poppler" ];
    "pq"                                 = [ "postgresql" ];
    "libpq"                              = [ "postgresql" ];
    "pthread"                            = [];
    "pulse"                              = [ "libpulseaudio" ];
    "pulse-simple"                       = [ "libpulseaudio" ];
    "python-3.3"                         = [ "python33" ];
    "python-3.4"                         = [ "python34" ];
    "Qt5Core"                            = [ "qt5" ];
    "Qt5Gui"                             = [ "qt5" ];
    "Qt5Qml"                             = [ "qt5" ];
    "Qt5Quick"                           = [ "qt5" ];
    "Qt5Widgets"                         = [ "qt5" ];
    "quadprog"                           = [ "QuadProgpp" ];
    "rt"                                 = []; # in glibc
    "rtlsdr"                             = [ "rtl-sdr" ];
    "ruby1.8"                            = [ "ruby" ];
    "sass"                               = [ "libsass" ];
    "sctp"                               = [ "lksctp-tools" ]; # This is linux-specific, we should create a common attribute if we ever add sctp support for other systems.
    "sdl2"                               = [ "SDL2" ];
    "sndfile"                            = [ "libsndfile" ];
    "sodium"                             = [ "libsodium" ];
    "sqlite3"                            = [ "sqlite" ];
    "ssh2"                               = [ "libssh2" ];
    "statgrab"                           = [ "libstatgrab" ];
    "stdc++"                             = []; # What is that?
    "stdc++.dll"                         = []; # What is that?
    "systemd-journal"                    = [ "systemd" ];
    "tag_c"                              = [ "taglib" ];
    "taglib_c"                           = [ "taglib" ];
    "tensorflow"                         = [ "libtensorflow" ];
    "udev"                               = [ "systemd" ];
    "uuid"                               = [ "libossp_uuid" ];
    "vte-2.91"                           = [ "vte_291" ];
    "wayland-client"                     = [ "wayland" ];
    "wayland-cursor"                     = [ "wayland" ];
    "wayland-egl"                        = [ "libGL" ];
    "wayland-server"                     = [ "wayland" ];
    "webkit2gtk"                         = [ "webkitgtk" ];
    "webkit2gtk-4.0"                     = [ "webkitgtk" ];
    "webkit2gtk-web-extension-4.0"       = [ "webkitgtk" ];
    "webkitgtk-3.0"                      = [ "webkitgtk24x-gtk3" ]; # These are the old APIs, of which 2.4 is the last provider, so map directly to that
    "vulkan"                             = [ "vulkan-loader" ]; # vulkan-loader provides vulkan.pc file for pkg-config.
    "xerces-c"                           = [ "xercesc" ];
    "xkbcommon"                          = [ "libxkbcommon" ];
    "xml2"                               = [ "libxml2" ];
    "yaml"                               = [ "libyaml" ];
    "yaml-0.1"                           = [ "libyaml" ];
    "z"                                  = [ "zlib" ];
    "zmq"                                = [ "zeromq" ];
} //
  lookupAttrsIn pkgs.xorg {
    "X11"                                = [ "libX11" ];
    "x11"                                = [ "xlibsWrapper" ];
    "xau"                                = [ "libXau" ];
    "Xcursor"                            = [ "libXcursor" ];
    "Xext"                               = [ "libXext" ];
    "xft"                                = [ "libXft" ];
    "Xi"                                 = [ "libXi" ];
    "Xinerama"                           = [ "libXinerama" ];
    "Xpm"                                = [ "libXpm" ];
    "Xrandr"                             = [ "libXrandr" ];
    "Xrender"                            = [ "libXrender" ];
    "Xss"                                = [ "libXScrnSaver" ];
    "Xtst"                               = [ "libXtst" ];
    "Xxf86vm"                            = [ "libXxf86vm" ];
} // {
    "gtkglext-1.0"                       =
      if pkgs ? gnome2 && pkgs.gnome2 ? gtkglext && pkgs ? gtk2
        then [ pkgs.gnome2.gtkglext pkgs.gtk2 ]
        else [];
    # This was renamed
    "gdk-pixbuf-2.0" =
      if pkgs ? gdk-pixbuf
        then [ pkgs.gdk-pixbuf ]
      else if pkgs ? gdk_pixbuf
        then [ pkgs.gdk_pixbuf ]
      else [];
}